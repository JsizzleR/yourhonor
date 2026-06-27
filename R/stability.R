#' Test-retest stability of a judge
#'
#' Re-grades the same items several times and reports how often the grade
#' changes. A judge whose verdicts wobble run to run is not a measurement
#' instrument yet; this puts a number on that wobble instead of discovering it
#' by accident in production.
#'
#' Two numbers are reported. `stability` is chance-corrected (Gwet's AC1 treating
#' the runs as raters), so a pure-noise judge scores near 0 rather than near the
#' percent-agreement chance floor. `percent_agreement` is the raw mean pairwise
#' agreement, kept for reference but not chance-corrected.
#'
#' @param data A data frame of items to grade. Data leads so it can be piped in.
#' @param judge A judge function `(data) -> character`, as in [calibrate_judge()].
#'   For the stability to be meaningful the judge should be stochastic (e.g. a
#'   non-zero temperature); a deterministic judge reports a flip rate of 0.
#' @param n_runs Number of repeated gradings.
#' @param seed Optional integer seed; the caller's random state is restored.
#'
#' @return An `yourhonor_stability` object with the per-item `modal` grade (NA on a
#'   tie), `flipped` and `tie` flags, the overall `flip_rate`, the chance-corrected
#'   `stability`, and the raw `percent_agreement`.
#'
#' @export
#' @examples
#' set.seed(1)
#' noisy_judge <- function(d) sample(c("C", "P"), nrow(d), replace = TRUE)
#' data <- tibble::tibble(input = paste("answer", 1:10))
#' test_retest(data, noisy_judge, n_runs = 4, seed = 1)
test_retest <- function(data, judge, n_runs = 5, seed = NULL) {
  if (!is.function(judge)) {
    cli::cli_abort("{.arg judge} must be a function to re-run for stability.")
  }
  data <- tibble::as_tibble(data)
  if (!is.null(seed)) withr::local_seed(seed)

  runs <- vapply(
    seq_len(n_runs),
    function(i) grade(judge, data),
    character(nrow(data))
  )
  runs <- matrix(runs, nrow = nrow(data), ncol = n_runs)

  per_item <- tibble::tibble(
    .row = seq_len(nrow(data)),
    modal = apply(runs, 1, modal_value),
    n_distinct = apply(runs, 1, function(z) length(unique(z))),
    flipped = apply(runs, 1, function(z) length(unique(z)) > 1),
    tie = apply(runs, 1, is_tie)
  )

  structure(
    list(
      per_item = per_item,
      runs = runs,
      flip_rate = mean(per_item$flipped),
      stability = chance_corrected_stability(runs),
      percent_agreement = pairwise_agreement(runs),
      n_runs = n_runs
    ),
    class = "yourhonor_stability"
  )
}

#' @export
print.yourhonor_stability <- function(x, ...) {
  cli::cli_h2("yourhonor test-retest stability")
  cli::cli_text(
    "{x$n_runs} runs - flip rate {fmt_num(x$flip_rate)}; ",
    "stability (Gwet AC1 across runs) {fmt_num(x$stability)}; ",
    "percent agreement {fmt_num(x$percent_agreement)}"
  )
  invisible(x)
}

# Modal label, NA on a tie (a 50/50 item has no confident mode) or when empty.
modal_value <- function(z) {
  z <- z[!is.na(z)]
  if (length(z) == 0) {
    return(NA_character_)
  }
  tab <- table(z)
  top <- names(tab)[tab == max(tab)]
  if (length(top) > 1) NA_character_ else top
}

is_tie <- function(z) {
  z <- z[!is.na(z)]
  if (length(z) == 0) {
    return(FALSE)
  }
  tab <- table(z)
  sum(tab == max(tab)) > 1
}

# Chance-corrected stability: Gwet's AC1 with the run columns as raters.
chance_corrected_stability <- function(runs) {
  if (ncol(runs) < 2) {
    return(NA_real_)
  }
  df <- as.data.frame(runs, stringsAsFactors = FALSE)
  tryCatch(
    as.numeric(irrCAC::gwet.ac1.raw(df)$est$coeff.val),
    error = function(e) NA_real_
  )
}

# Raw mean percent agreement over all pairs of runs (has a chance floor).
pairwise_agreement <- function(runs) {
  k <- ncol(runs)
  if (k < 2) {
    return(NA_real_)
  }
  agrees <- numeric(0)
  for (i in seq_len(k - 1)) {
    for (j in seq(i + 1, k)) {
      agrees <- c(agrees, mean(runs[, i] == runs[, j]))
    }
  }
  mean(agrees)
}
