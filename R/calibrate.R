#' Calibrate an LLM-as-judge against human labels
#'
#' The core entry point. Runs a judge over a set of human-labeled examples and
#' reports how well the judge agrees with the humans, where it diverges, and how
#' the labels are distributed. This is what turns "we graded with an LLM judge"
#' into "we graded with a judge that agrees with humans at AC1 = 0.81".
#'
#' @section The judge contract:
#' `judge` is a function of one argument that takes the whole `gold` tibble and
#' returns a character vector of labels, one per row. This vectorised contract
#' makes it easy to wrap an `ellmer` chat or a `vitals` model-graded scorer (see
#' `vignette("yourhonor")`). To calibrate grades you already have, leave `judge`
#' as `NULL` and include a `judge_label` column in `gold`.
#'
#' @param gold A data frame of labeled examples. Must contain the column(s) named
#'   in `human_cols`; when `judge` is a function it will typically also use
#'   `input` and `target` columns. Labels may be any categorical scale (e.g.
#'   `"C"`/`"P"`/`"I"`). Data leads so that a gold tibble can be piped in.
#' @param judge A function `(gold) -> character` returning one label per row, or
#'   `NULL` to use an existing `judge_label` column in `gold`.
#' @param human_cols The human-rater column(s). One column (default
#'   `"human_label"`) compares the judge to that rater. Two or more columns
#'   compare the judge to the human *consensus* (per-item majority, `NA` on a
#'   tie) and additionally report the human-human agreement *ceiling* -- you
#'   cannot expect a judge to agree with people more than they agree with each
#'   other.
#' @param coefficients,conf_level,n_boot,seed Passed to [agreement()].
#'
#' @return A `yourhonor_calibration` object: a list with the `agreement` table,
#'   the `confusion` tibble, the human-human `human_ceiling` (when `human_cols`
#'   has more than one column), the per-item join (`items`), and the `ratings`.
#'
#' @seealso [agreement()], [confusion()], [probe_bias()], [report()].
#' @export
#' @examples
#' gold <- tibble::tibble(
#'   input = paste("answer", 1:8),
#'   target = "is it correct?",
#'   human_label = c("C", "C", "P", "I", "C", "P", "I", "C"),
#'   judge_label = c("C", "C", "C", "I", "C", "P", "P", "C")
#' )
#' # calibrate grades that already exist
#' cal <- calibrate_judge(gold, n_boot = 200, seed = 1)
#' cal
calibrate_judge <- function(gold,
                            judge = NULL,
                            human_cols = "human_label",
                            coefficients = yourhonor_coefficients,
                            conf_level = 0.95,
                            n_boot = 2000,
                            seed = NULL) {
  gold <- tibble::as_tibble(gold)
  if (!all(human_cols %in% names(gold))) {
    missing <- setdiff(human_cols, names(gold))
    cli::cli_abort("{.arg gold} is missing human rater column{?s}: {.field {missing}}.")
  }
  coefficients <- rlang::arg_match(coefficients, yourhonor_coefficients, multiple = TRUE)

  judge_label <- grade(judge, gold)
  human_mat <- as.data.frame(
    lapply(gold[human_cols], as.character),
    stringsAsFactors = FALSE
  )
  human <- if (length(human_cols) > 1) {
    apply(human_mat, 1, modal_value)
  } else {
    human_mat[[1]]
  }

  ratings_full <- data.frame(
    human = as.character(human),
    judge = as.character(judge_label),
    stringsAsFactors = FALSE
  )
  complete <- stats::complete.cases(ratings_full)
  if (any(!complete)) {
    cli::cli_warn(
      "{sum(!complete)} item{?s} had a missing label and were excluded from the agreement estimate."
    )
  }
  ratings <- ratings_full[complete, , drop = FALSE]
  if (nrow(ratings) < 2) {
    cli::cli_abort("Need at least 2 fully-labeled items to calibrate.")
  }

  agr <- agreement(
    ratings,
    coefficients = coefficients,
    conf_level = conf_level,
    n_boot = n_boot,
    seed = seed
  )
  conf <- confusion(ratings)

  human_ceiling <- NULL
  if (length(human_cols) > 1) {
    hm <- human_mat[stats::complete.cases(human_mat), , drop = FALSE]
    if (nrow(hm) >= 2) {
      human_ceiling <- compute_agreement(
        hm, multirater_coefficients, conf_level, n_boot, seed
      )
    }
  }

  items <- gold
  items$judge_label <- judge_label
  if (length(human_cols) > 1) items$human_consensus <- human
  items$agree <- ratings_full$human == ratings_full$judge
  items <- tibble::as_tibble(items)

  structure(
    list(
      agreement = agr,
      confusion = conf,
      human_ceiling = human_ceiling,
      items = items,
      ratings = ratings,
      coefficients = coefficients,
      conf_level = conf_level,
      n_boot = n_boot,
      seed = seed,
      n_items = nrow(ratings),
      n_excluded = sum(!complete),
      human_cols = human_cols
    ),
    class = "yourhonor_calibration"
  )
}

#' @export
print.yourhonor_calibration <- function(x, ...) {
  cli::cli_h1("yourhonor judge calibration")
  cli::cli_text(
    "{x$n_items} item{?s} - judge-vs-human agreement ",
    "({round(x$conf_level * 100)}% bootstrap CI):"
  )
  ag <- x$agreement
  cli::cli_ul()
  for (i in seq_len(nrow(ag))) {
    cli::cli_li(
      "{.strong {ag$coefficient[i]}}: {fmt_num(ag$estimate[i])} [{fmt_num(ag$conf_low[i])}, {fmt_num(ag$conf_high[i])}] ({ag$interpretation[i]})"
    )
  }
  cli::cli_end()
  n_disagree <- sum(!x$items$agree, na.rm = TRUE)
  cli::cli_text(
    "{n_disagree} disagreement{?s}; see {.code confusion()} for where they fall."
  )
  if (!is.null(x$human_ceiling)) {
    hc <- x$human_ceiling
    ac1 <- hc$estimate[hc$coefficient == "gwet_ac1"]
    cli::cli_text(
      "Human-human ceiling ({length(x$human_cols)} raters): AC1 = {fmt_num(ac1)} ",
      "(the judge cannot be expected to agree more than the humans do)."
    )
  }
  if (!is.null(x$n_excluded) && x$n_excluded > 0) {
    cli::cli_text("{x$n_excluded} item{?s} excluded for a missing label.")
  }
  invisible(x)
}

# Run the judge, or reuse existing grades. Internal.
grade <- function(judge, data) {
  if (is.null(judge)) {
    if (!"judge_label" %in% names(data)) {
      cli::cli_abort(
        "With {.code judge = NULL}, {.arg gold} must contain a {.field judge_label} column."
      )
    }
    return(as.character(data$judge_label))
  }
  if (!is.function(judge)) {
    cli::cli_abort("{.arg judge} must be a function or {.code NULL}.")
  }
  out <- judge(data)
  if (length(out) != nrow(data)) {
    cli::cli_abort(
      "The judge returned {length(out)} label{?s} for {nrow(data)} row{?s}; \\
      it must return one label per row."
    )
  }
  as.character(out)
}
