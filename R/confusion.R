#' Judge-versus-human confusion matrix
#'
#' Shows where a judge and humans disagree. Label order is preserved from
#' `label_levels` rather than sorted alphabetically.
#'
#' @param x A `yourhonor_calibration` object or a ratings data frame holding
#'   one human column and one judge column (see `human_col` and `judge_col`).
#' @param normalize One of `"none"`, `"row"`, or `"col"`.
#' @param label_levels Complete label universe in display order. A calibration
#'   object's stored levels are used by default.
#' @param conf_level Confidence level for exact per-class binomial intervals.
#' @param human_col,judge_col Names of the human and judge columns of a ratings
#'   data frame. Both must be given together. When both are `NULL`, columns
#'   named `human` and `judge` are used if present; otherwise a data frame with
#'   exactly two columns is read as human then judge. A message (class
#'   `yourhonor_inferred_columns`) reports the choice whenever inference
#'   ignored columns or read them positionally. Cannot be combined with a
#'   calibration object, which carries its own pairing.
#'
#' @return A tibble with `human_label`, `judge_label`, `n`, and `prop`, in
#'   `label_levels` order. The resolved level order is attached as the
#'   `"label_levels"` attribute. Per-class metrics remain attached for
#'   compatibility and are also available through [confusion_metrics()].
#' @export
#' @examples
#' ratings <- data.frame(
#'   human = c("C", "C", "P", "I"),
#'   judge = c("C", "P", "P", "I")
#' )
#' confusion(ratings, normalize = "row", label_levels = c("I", "P", "C"),
#'           human_col = "human", judge_col = "judge")
confusion <- function(x,
                      normalize = c("none", "row", "col"),
                      label_levels = NULL,
                      conf_level = 0.95,
                      human_col = NULL,
                      judge_col = NULL) {
  build_confusion(
    x, normalize, label_levels, conf_level, human_col, judge_col,
    call = rlang::current_env()
  )
}

confusion_normalizations <- c("none", "row", "col")

# Shared by confusion() and confusion_metrics(); `call` names whichever public
# function the user invoked so validation errors point at it.
build_confusion <- function(x,
                            normalize,
                            label_levels,
                            conf_level,
                            human_col,
                            judge_col,
                            call = rlang::caller_env()) {
  normalize <- rlang::arg_match(
    normalize, confusion_normalizations, error_arg = "normalize",
    error_call = call
  )
  conf_level <- check_probability(
    conf_level, "conf_level", open = TRUE, call = call
  )
  if (inherits(x, "yourhonor_calibration") && is.null(label_levels)) {
    label_levels <- x$label_levels
  }
  ratings <- as_confusion_ratings(x, human_col, judge_col, call = call)
  label_levels <- resolve_label_levels(
    ratings, label_levels, allow_single = TRUE, arg = "x", call = call
  )
  ratings <- ratings[stats::complete.cases(ratings), , drop = FALSE]
  if (nrow(ratings) == 0L) {
    cli::cli_abort(
      "No complete judge-human pairs are available for a confusion matrix.",
      call = call
    )
  }

  human <- factor(as.character(ratings[[1]]), levels = label_levels)
  judge <- factor(as.character(ratings[[2]]), levels = label_levels)
  tab <- table(human_label = human, judge_label = judge)
  df <- as.data.frame(tab, stringsAsFactors = FALSE)
  names(df)[names(df) == "Freq"] <- "n"

  denom <- switch(
    normalize,
    none = sum(df$n),
    row = ave(df$n, df$human_label, FUN = sum),
    col = ave(df$n, df$judge_label, FUN = sum)
  )
  prop <- df$n / denom
  prop[!is.finite(prop)] <- 0
  df$prop <- prop

  out <- tibble::as_tibble(df)
  attr(out, "label_levels") <- label_levels
  attr(out, "metrics") <- class_metrics(tab, conf_level)
  out
}

# A two-column data frame ordered human then judge. Calibration objects carry
# their own pairing; raw frames are selected explicitly or by inference.
as_confusion_ratings <- function(x,
                                 human_col = NULL,
                                 judge_col = NULL,
                                 call = rlang::caller_env()) {
  if (inherits(x, "yourhonor_calibration")) {
    check_no_column_selection(human_col, judge_col, call = call)
    return(x$ratings)
  }
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  check_unique_column_names(x, call = call)
  if (is.null(human_col) != is.null(judge_col)) {
    cli::cli_abort(
      "Supply both {.arg human_col} and {.arg judge_col}, or neither.",
      call = call
    )
  }
  if (is.null(human_col)) {
    cols <- infer_confusion_cols(x, call = call)
  } else {
    human_col <- check_column_name(human_col, "human_col", call = call)
    judge_col <- check_column_name(judge_col, "judge_col", call = call)
    if (identical(human_col, judge_col)) {
      cli::cli_abort(
        "{.arg human_col} and {.arg judge_col} must name different columns.",
        call = call
      )
    }
    cols <- c(human_col, judge_col)
    check_columns_present(x, cols, "human_col/judge_col", call = call)
  }
  x[, cols, drop = FALSE]
}

# A calibration object or a confusion tibble carries its own pairing, so an
# explicit selection on one would be silently ignored; agreement() refuses
# `rater_cols` the same way. `what` names the object the caller passed.
check_no_column_selection <- function(human_col,
                                      judge_col,
                                      what = "a calibration object",
                                      call = rlang::caller_env()) {
  if (!is.null(human_col) || !is.null(judge_col)) {
    cli::cli_abort(
      "{.arg human_col} and {.arg judge_col} cannot be combined with {what}.",
      call = call
    )
  }
  invisible(NULL)
}

# Name-based selection on a frame with repeated names would silently take the
# first match, so such frames are refused before any column is chosen.
check_unique_column_names <- function(x,
                                      arg = "x",
                                      call = rlang::caller_env()) {
  repeated <- unique(names(x)[duplicated(names(x))])
  if (length(repeated) > 0L) {
    cli::cli_abort(
      paste0(
        "{.arg {arg}} has duplicated column ",
        "{cli::qty(length(repeated))}name{?s}: {.field {repeated}}."
      ),
      call = call
    )
  }
  invisible(x)
}

infer_confusion_cols <- function(x, call = rlang::caller_env()) {
  default_pair <- c("human", "judge")
  selectors <- c("human_col", "judge_col")
  if (all(default_pair %in% names(x))) {
    ignored <- setdiff(names(x), default_pair)
    if (length(ignored) > 0L) {
      inform_inferred_cols(
        c(
          "Using {.field human} and {.field judge} as the rating columns.",
          "Ignoring column{?s}: {.field {ignored}}."
        ),
        selectors
      )
    }
    return(default_pair)
  }
  if (ncol(x) == 2L) {
    inform_inferred_cols(
      paste0(
        "Reading {.field {names(x)[[1]]}} as human and ",
        "{.field {names(x)[[2]]}} as judge."
      ),
      selectors
    )
    return(names(x))
  }
  cli::cli_abort(
    c(
      "{.fn confusion} needs one human and one judge column.",
      "i" = paste0(
        "Supply {.arg human_col} and {.arg judge_col} to pick them from ",
        "{ncol(x)} columns."
      )
    ),
    call = call
  )
}

#' Per-class judge metrics
#'
#' Returns class support, predicted support, precision, and recall with humans
#' treated as the reference. Unlike a hidden tibble attribute, this accessor is
#' stable across ordinary serialization and reporting workflows.
#'
#' @param x A `yourhonor_calibration` object, a confusion tibble returned by
#'   [confusion()], or a ratings data frame.
#' @param label_levels Optional complete label universe in display order. For
#'   a calibration object or a confusion tibble it must contain every label
#'   observed there; the stored per-class rows are returned in that order,
#'   with zero-support rows for labels not seen.
#' @param human_col,judge_col Passed to [confusion()] for a ratings data frame.
#'   Cannot be combined with a calibration object or a confusion tibble.
#' @return A tibble with one row per label.
#' @export
confusion_metrics <- function(x,
                              label_levels = NULL,
                              human_col = NULL,
                              judge_col = NULL) {
  call <- rlang::current_env()
  is_calibration <- inherits(x, "yourhonor_calibration")
  cached <- if (is_calibration) {
    x$confusion_metrics
  } else {
    attr(x, "metrics", exact = TRUE)
  }
  if (!is.null(cached)) {
    what <- if (is_calibration) "a calibration object" else "a confusion tibble"
    check_no_column_selection(human_col, judge_col, what = what, call = call)
    return(reindex_class_metrics(cached, label_levels, call = call))
  }
  conf <- build_confusion(
    x, "none", label_levels, 0.95, human_col, judge_col, call = call
  )
  attr(conf, "metrics", exact = TRUE)
}

# Per-class rows do not depend on the other classes (an unobserved label has
# an all-zero row and column), so a requested universe only reorders the
# cached rows, adds zero-support rows for labels never seen, and drops labels
# that were declared but never observed.
reindex_class_metrics <- function(metrics,
                                  label_levels,
                                  call = rlang::caller_env()) {
  if (is.null(label_levels)) return(metrics)
  observed <- metrics[metrics$support > 0L | metrics$predicted > 0L, ]
  label_levels <- resolve_label_levels(
    observed["label"], label_levels, allow_single = TRUE,
    arg = "x", call = call
  )
  index <- match(label_levels, metrics$label)
  out <- metrics[index, ]
  out$label <- label_levels
  out$support[is.na(index)] <- 0L
  out$predicted[is.na(index)] <- 0L
  out
}

class_metrics <- function(tab, conf_level = 0.95) {
  hit <- diag(tab)
  support <- rowSums(tab)
  predicted <- colSums(tab)
  recall <- ifelse(support > 0, hit / support, NA_real_)
  precision <- ifelse(predicted > 0, hit / predicted, NA_real_)
  recall_ci <- t(vapply(seq_along(hit), function(i) {
    exact_binom_interval(hit[[i]], support[[i]], conf_level)
  }, numeric(2)))
  precision_ci <- t(vapply(seq_along(hit), function(i) {
    exact_binom_interval(hit[[i]], predicted[[i]], conf_level)
  }, numeric(2)))
  tibble::tibble(
    label = rownames(tab),
    support = as.integer(support),
    predicted = as.integer(predicted),
    precision = as.numeric(precision),
    precision_conf_low = precision_ci[, 1],
    precision_conf_high = precision_ci[, 2],
    recall = as.numeric(recall),
    recall_conf_low = recall_ci[, 1],
    recall_conf_high = recall_ci[, 2]
  )
}
