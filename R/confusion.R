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
#'   ignored columns or read them positionally. Ignored for calibration
#'   objects.
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
  normalize <- rlang::arg_match(normalize)
  conf_level <- check_probability(conf_level, open = TRUE)
  if (inherits(x, "yourhonor_calibration") && is.null(label_levels)) {
    label_levels <- x$label_levels
  }
  ratings <- as_confusion_ratings(x, human_col, judge_col)
  label_levels <- resolve_label_levels(ratings, label_levels, allow_single = TRUE, arg = "x")
  ratings <- ratings[stats::complete.cases(ratings), , drop = FALSE]
  if (nrow(ratings) == 0L) {
    cli::cli_abort("No complete judge-human pairs are available for a confusion matrix.")
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
  if (inherits(x, "yourhonor_calibration")) return(x$ratings)
  x <- as.data.frame(x, stringsAsFactors = FALSE)
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

infer_confusion_cols <- function(x, call = rlang::caller_env()) {
  default_pair <- c("human", "judge")
  if (all(default_pair %in% names(x))) {
    ignored <- setdiff(names(x), default_pair)
    if (length(ignored) > 0L) {
      inform_inferred_cols(
        c(
          "Using {.field human} and {.field judge} as the rating columns.",
          "Ignoring column{?s}: {.field {ignored}}."
        ),
        "human_col"
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
      "human_col"
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
#' @param label_levels Optional complete label universe in display order.
#' @param human_col,judge_col Passed to [confusion()] for a ratings data frame.
#' @return A tibble with one row per label.
#' @export
confusion_metrics <- function(x,
                              label_levels = NULL,
                              human_col = NULL,
                              judge_col = NULL) {
  if (inherits(x, "yourhonor_calibration")) {
    return(x$confusion_metrics)
  }
  metrics <- attr(x, "metrics", exact = TRUE)
  if (!is.null(metrics)) return(metrics)
  conf <- confusion(
    x, label_levels = label_levels, human_col = human_col, judge_col = judge_col
  )
  attr(conf, "metrics", exact = TRUE)
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
