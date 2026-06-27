#' Judge-versus-human confusion matrix
#'
#' Shows *where* a judge and humans disagree, not just how much. Treating the
#' human labels as truth, it returns the tidy contingency of human label by
#' judge label, and attaches per-class precision and recall of the judge so you
#' can see, for example, that a judge over-awards partial credit rather than
#' failing good answers.
#'
#' @param x An `yourhonor_calibration` object or a two-column ratings data frame
#'   (see [agreement()]).
#' @param normalize One of `"none"` (proportions of the whole table), `"row"`
#'   (within each human label) or `"col"` (within each judge label).
#'
#' @return A tibble with columns `human_label`, `judge_label`, `n` and `prop`.
#'   Per-class precision and recall are attached as the `"metrics"` attribute.
#'
#' @export
#' @examples
#' ratings <- data.frame(
#'   human = c("C", "C", "P", "I", "C", "P", "I", "C"),
#'   judge = c("C", "C", "C", "I", "C", "P", "P", "C")
#' )
#' confusion(ratings, normalize = "row")
confusion <- function(x, normalize = c("none", "row", "col")) {
  normalize <- rlang::arg_match(normalize)
  ratings <- as_ratings(x)
  levels <- sort(unique(c(ratings[[1]], ratings[[2]])))
  human <- factor(ratings[[1]], levels = levels)
  judge <- factor(ratings[[2]], levels = levels)

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
  attr(out, "metrics") <- class_metrics(tab)
  out
}

# Per-class precision and recall, humans as truth.
class_metrics <- function(tab) {
  hit <- diag(tab)
  recall <- ifelse(rowSums(tab) > 0, hit / rowSums(tab), NA_real_)
  precision <- ifelse(colSums(tab) > 0, hit / colSums(tab), NA_real_)
  tibble::tibble(
    label = rownames(tab),
    precision = as.numeric(precision),
    recall = as.numeric(recall)
  )
}
