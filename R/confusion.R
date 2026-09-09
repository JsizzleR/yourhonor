#' Judge-versus-human confusion matrix
#'
#' Shows where a judge and humans disagree. Label order is preserved from
#' `label_levels` rather than sorted alphabetically.
#'
#' @param x A `yourhonor_calibration` object or a two-column ratings data frame.
#' @param normalize One of `"none"`, `"row"`, or `"col"`.
#' @param label_levels Complete label universe in display order. A calibration
#'   object's stored levels are used by default.
#' @param conf_level Confidence level for exact per-class binomial intervals.
#'
#' @return A tibble with `human_label`, `judge_label`, `n`, and `prop`.
#'   Per-class metrics remain attached for compatibility and are also available
#'   through [confusion_metrics()].
#' @export
#' @examples
#' ratings <- data.frame(
#'   human = c("C", "C", "P", "I"),
#'   judge = c("C", "P", "P", "I")
#' )
#' confusion(ratings, normalize = "row", label_levels = c("I", "P", "C"))
confusion <- function(x,
                      normalize = c("none", "row", "col"),
                      label_levels = NULL,
                      conf_level = 0.95) {
  normalize <- rlang::arg_match(normalize)
  conf_level <- check_probability(conf_level, "conf_level", open = TRUE)
  if (inherits(x, "yourhonor_calibration") && is.null(label_levels)) {
    label_levels <- x$label_levels
  }
  ratings <- as_ratings(x)
  if (ncol(ratings) != 2L) {
    cli::cli_abort("{.fn confusion} requires exactly two rating columns.")
  }
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
  attr(out, "metrics") <- class_metrics(tab, conf_level)
  out
}

#' Per-class judge metrics
#'
#' Returns class support, predicted support, precision, and recall with humans
#' treated as the reference. Unlike a hidden tibble attribute, this accessor is
#' stable across ordinary serialization and reporting workflows.
#'
#' @param x A `yourhonor_calibration` object, a confusion tibble returned by
#'   [confusion()], or a two-column ratings data frame.
#' @param label_levels Optional complete label universe in display order.
#' @return A tibble with one row per label.
#' @export
confusion_metrics <- function(x, label_levels = NULL) {
  if (inherits(x, "yourhonor_calibration")) {
    return(x$confusion_metrics)
  }
  metrics <- attr(x, "metrics", exact = TRUE)
  if (!is.null(metrics)) return(metrics)
  attr(confusion(x, label_levels = label_levels), "metrics", exact = TRUE)
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
