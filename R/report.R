#' One-paragraph "is my judge usable?" verdict
#'
#' Summarises a calibration (and, optionally, bias and stability results) into a
#' compact verdict you can drop next to a `vitals` eval or into a README: the
#' headline agreement coefficient with its interval, any significant biases, the
#' retest flip rate, and a single overall call.
#'
#' The overall call is intentionally conservative: a judge is only `"USE"` when
#' agreement is strong, no probed bias is significant, and re-runs are stable.
#'
#' @param x An `yourhonor_calibration` object from [calibrate_judge()].
#' @param bias Optional [probe_bias()] result.
#' @param stability Optional [test_retest()] result.
#' @param output `"console"` to print a formatted summary, or `"md"` to return a
#'   Markdown string.
#'
#' @return For `output = "md"`, an `yourhonor_report` (a Markdown string) returned
#'   invisibly. For `"console"`, `x` invisibly, after printing.
#'
#' @export
#' @examples
#' gold <- tibble::tibble(
#'   human_label = c("C", "C", "P", "I", "C", "P", "I", "C"),
#'   judge_label = c("C", "C", "C", "I", "C", "P", "P", "C")
#' )
#' cal <- calibrate_judge(gold, n_boot = 200, seed = 1)
#' report(cal, output = "md")
report <- function(x, bias = NULL, stability = NULL, output = c("console", "md")) {
  if (!inherits(x, "yourhonor_calibration")) {
    cli::cli_abort("{.arg x} must be an {.cls yourhonor_calibration} object.")
  }
  output <- rlang::arg_match(output)

  head <- headline_row(x)
  biases <- significant_biases(bias)
  flip <- if (is.null(stability)) NA_real_ else stability$flip_rate
  call <- overall_verdict(head$estimate, length(biases) == 0, flip)

  if (output == "md") {
    md <- build_markdown(x, head, biases, flip, call)
    return(invisible(structure(md, class = "yourhonor_report")))
  }

  cli::cli_h1("yourhonor verdict: {call}")
  cli::cli_text(
    "Agreement: {.strong {head$coefficient}} = {fmt_num(head$estimate)} ",
    "[{fmt_num(head$conf_low)}, {fmt_num(head$conf_high)}] ({head$interpretation})."
  )
  ceil <- ceiling_ac1(x)
  if (!is.na(ceil)) {
    cli::cli_text("Human-human ceiling: AC1 = {fmt_num(ceil)}.")
  }
  if (length(biases) > 0) {
    cli::cli_text("Significant bias: {.strong {paste(biases, collapse = ', ')}}.")
  } else if (!is.null(bias)) {
    cli::cli_text("Bias probes: none significant.")
  }
  if (!is.na(flip)) {
    cli::cli_text("Test-retest flip rate: {fmt_num(flip)}.")
  }
  invisible(x)
}

#' @export
print.yourhonor_report <- function(x, ...) {
  cat(unclass(x), sep = "\n")
  invisible(x)
}

# Prefer Gwet's AC1 as the headline (robust to label skew), else the first row.
# The overall verdict's 0.8 threshold is tuned for AC1, so flag the fallback.
headline_row <- function(x) {
  ag <- x$agreement
  i <- which(ag$coefficient == "gwet_ac1")
  if (length(i) == 0) {
    cli::cli_inform(
      "Headline uses {.val {ag$coefficient[1]}}; {.val gwet_ac1} was not computed."
    )
    i <- 1L
  }
  as.list(ag[i[1], ])
}

significant_biases <- function(bias) {
  if (is.null(bias)) {
    return(character(0))
  }
  bias$probe[!is.na(bias$verdict) & bias$verdict == "BIAS"]
}

# Human-human ceiling AC1, or NA when only one human rater was supplied.
ceiling_ac1 <- function(x) {
  hc <- x$human_ceiling
  if (is.null(hc)) {
    return(NA_real_)
  }
  v <- hc$estimate[hc$coefficient == "gwet_ac1"]
  if (length(v) == 0) NA_real_ else v
}

overall_verdict <- function(estimate, no_bias, flip_rate) {
  stable <- is.na(flip_rate) || flip_rate <= 0.1
  if (!is.na(estimate) && estimate >= 0.8 && no_bias && stable) {
    "USE"
  } else if (!is.na(estimate) && estimate >= 0.6) {
    "USE WITH CAUTION"
  } else {
    "NOT CALIBRATED"
  }
}

build_markdown <- function(x, head, biases, flip, call) {
  lines <- c(
    sprintf("## Judge calibration (n = %d)", x$n_items),
    "",
    sprintf(
      "Agreement with humans: **%s = %s** [%s, %s] (%s).",
      head$coefficient, fmt_num(head$estimate),
      fmt_num(head$conf_low), fmt_num(head$conf_high), head$interpretation
    )
  )
  ceil <- ceiling_ac1(x)
  if (!is.na(ceil)) {
    lines <- c(lines, sprintf("Human-human ceiling: AC1 = %s.", fmt_num(ceil)))
  }
  if (!is.na(flip)) {
    lines <- c(lines, sprintf("Test-retest flip rate: %s.", fmt_num(flip)))
  }
  if (length(biases) > 0) {
    lines <- c(lines, sprintf("**Significant bias: %s.**", paste(biases, collapse = ", ")))
  }
  lines <- c(lines, "", sprintf("Overall: **%s**.", call))
  lines
}
