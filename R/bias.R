#' Probe a judge for paired bias
#'
#' Re-grades each example after a controlled change that a fair judge should
#' ignore, and measures how often the verdict flips and in which direction.
#' Because each item is its own control, the test is paired and the direction is
#' tested with an exact binomial sign test on the discordant items. With several
#' probes the p-values are corrected for multiple comparisons (`p_adjust`).
#'
#' Every probe is built as a length-matched control/treatment pair, so a flip
#' reflects the construct being probed and not a confound with answer length:
#' \describe{
#'   \item{`verbosity`}{Pads the answer with neutral filler. Here the change
#'     deliberately *does* alter length; that is the construct.}
#'   \item{`self_preference`}{"written by self" vs "written by peer" -- the two
#'     prefixes are the same length, so only the authorship claim varies.}
#'   \item{`bandwagon`}{"most reviewers said this looks right" vs "...wrong" --
#'     equal length, so only the claimed crowd verdict varies. A judge that
#'     follows the crowd drifts toward the claimed verdict.}
#'   \item{`position`}{Swaps "Answer A" / "Answer B" segments. Only meaningful for
#'     pairwise inputs; for a pointwise grader with no A/B structure it is a no-op
#'     (flip rate 0) and `probe_bias()` says so.}
#' }
#'
#' @param data A data frame with at least an `input` column to perturb. Data
#'   leads so a tibble can be piped in.
#' @param judge A judge function `(data) -> character`, as in [calibrate_judge()].
#'   A function is required (not pre-graded labels) because the perturbed inputs
#'   must actually be re-graded.
#' @param probes Character vector; any of `"verbosity"`, `"self_preference"`,
#'   `"bandwagon"`, `"position"`. Defaults to the three that are valid for a
#'   pointwise grader.
#' @param ordered_levels The label scale from worst to best, used to score the
#'   direction of a flip. Defaults to `c("I", "P", "C")`.
#' @param n Optional number of items to sample (for speed); `NULL` uses all.
#' @param seed Optional integer seed; the caller's random state is restored.
#' @param flip_threshold Minimum flip rate, alongside `p_value_adj < alpha`, for a
#'   probe to be flagged as `"BIAS"` rather than `"ok"`.
#' @param alpha Significance level for the per-probe sign test.
#' @param p_adjust Multiple-comparison correction across probes, passed to
#'   [stats::p.adjust()]. One of `"holm"` (default), `"none"`, `"BH"`,
#'   `"bonferroni"`.
#'
#' @return A `yourhonor_bias` tibble with one row per probe: `flip_rate`,
#'   `direction`, `statistic`, `n_discordant`, `p_value`, `p_value_adj` and
#'   `verdict` (`NA` when no item could be scored).
#'
#' @export
#' @examples
#' # a judge that rewards longer answers -> verbosity bias
#' length_judge <- function(d) ifelse(nchar(d$input) > 40, "C", "P")
#' data <- tibble::tibble(input = paste("answer", 1:20))
#' probe_bias(data, length_judge, probes = "verbosity", seed = 1)
probe_bias <- function(data,
                       judge,
                       probes = c("verbosity", "self_preference", "bandwagon"),
                       ordered_levels = c("I", "P", "C"),
                       n = NULL,
                       seed = NULL,
                       flip_threshold = 0.1,
                       alpha = 0.05,
                       p_adjust = c("holm", "none", "BH", "bonferroni")) {
  if (!is.function(judge)) {
    cli::cli_abort("{.arg judge} must be a function; bias probes re-grade perturbed inputs.")
  }
  probes <- rlang::arg_match(
    probes, c("verbosity", "self_preference", "bandwagon", "position"), multiple = TRUE
  )
  p_adjust <- rlang::arg_match(p_adjust)
  data <- tibble::as_tibble(data)
  if (!"input" %in% names(data)) {
    cli::cli_abort("{.arg data} must contain an {.field input} column to perturb.")
  }
  if (!is.null(seed)) withr::local_seed(seed)
  if (!is.null(n)) {
    if (n >= nrow(data)) {
      cli::cli_warn("Requested n = {n} but data has {nrow(data)} row{?s}; using all rows.")
    } else {
      data <- data[sort(sample.int(nrow(data), n)), , drop = FALSE]
    }
  }
  if ("position" %in% probes && !any(has_ab(data$input))) {
    cli::cli_inform(
      "Probe {.val position} needs inputs with 'Answer A:'/'Answer B:' structure; it is a no-op here."
    )
  }

  base_grade <- to_ordinal(grade(judge, data), ordered_levels)
  if (all(is.na(base_grade))) {
    cli::cli_abort(
      "No graded labels matched {.arg ordered_levels}; pass the label scale your judge uses."
    )
  }

  raw <- lapply(probes, function(p) {
    pair <- probe_pair(data$input, p)
    before <- if (pair$control_is_identity) {
      base_grade
    } else {
      to_ordinal(grade(judge, set_input(data, pair$control)), ordered_levels)
    }
    after <- to_ordinal(grade(judge, set_input(data, pair$treatment)), ordered_levels)
    c(list(probe = p), paired_flip_test(before, after))
  })

  p_value <- vapply(raw, function(r) r$p_value, numeric(1))
  p_value_adj <- p_value
  scored <- !is.na(p_value)
  if (p_adjust != "none" && any(scored)) {
    p_value_adj[scored] <- stats::p.adjust(p_value[scored], method = p_adjust)
  }

  rows <- lapply(seq_along(raw), function(i) {
    r <- raw[[i]]
    padj <- p_value_adj[i]
    verdict <- if (is.na(r$flip_rate)) {
      NA_character_
    } else if (!is.na(padj) && padj < alpha && r$flip_rate > flip_threshold) {
      "BIAS"
    } else {
      "ok"
    }
    tibble::tibble(
      probe = r$probe,
      flip_rate = r$flip_rate,
      direction = r$direction,
      statistic = r$statistic,
      n_discordant = r$n_discordant,
      p_value = r$p_value,
      p_value_adj = padj,
      verdict = verdict
    )
  })

  out <- do.call(vctrs::vec_rbind, rows)
  structure(out, class = c("yourhonor_bias", class(out)))
}

#' @export
print.yourhonor_bias <- function(x, ...) {
  cli::cli_h2("yourhonor bias probes")
  NextMethod()
}

set_input <- function(data, input) {
  data$input <- input
  data
}

has_ab <- function(x) grepl("Answer [AB]:", x)

# Each probe defines a control and a treatment input; a fair judge grades them
# the same. control_is_identity = TRUE means the control is the untouched input,
# so its grading can be reused from the baseline.
probe_pair <- function(input, type) {
  switch(
    type,
    verbosity = list(
      control = input,
      treatment = paste0(input, "\n\n", verbosity_filler()),
      control_is_identity = TRUE
    ),
    position = list(
      control = input,
      treatment = swap_positions(input),
      control_is_identity = TRUE
    ),
    self_preference = list(
      control = paste0(self_pref_prefix("peer"), input),
      treatment = paste0(self_pref_prefix("self"), input),
      control_is_identity = FALSE
    ),
    bandwagon = list(
      control = paste0(bandwagon_prefix("wrong"), input),
      treatment = paste0(bandwagon_prefix("right"), input),
      control_is_identity = FALSE
    ),
    cli::cli_abort("Unknown probe {.val {type}}.")
  )
}

# "peer"/"self" are both four letters, so the two prefixes are identical in
# length: only the authorship claim varies, never the input length.
self_pref_prefix <- function(which) {
  paste0("(For context: the response below was written by ", which, ".)\n\n")
}

# "right"/"wrong" are both five letters, so only the claimed crowd verdict varies.
bandwagon_prefix <- function(verdict) {
  paste0("(Most other reviewers said this answer looks ", verdict, ".)\n\n")
}

verbosity_filler <- function() {
  paste(
    "To elaborate further and provide additional context, it is worth noting",
    "that the considerations above can be examined from several complementary",
    "angles, each of which reinforces the overall point without altering its",
    "substance in any material way whatsoever."
  )
}

# Swap the first two "Answer A:" / "Answer B:" segments when present.
swap_positions <- function(x) {
  vapply(x, function(s) {
    m <- regmatches(s, gregexpr("Answer [AB]:.*?(?=Answer [AB]:|$)", s, perl = TRUE))[[1]]
    if (length(m) >= 2) {
      m[c(1, 2)] <- m[c(2, 1)]
      paste(m, collapse = "")
    } else {
      s
    }
  }, character(1), USE.NAMES = FALSE)
}

# Map labels to integer ranks on the ordered scale.
to_ordinal <- function(labels, ordered_levels) {
  idx <- match(as.character(labels), ordered_levels)
  if (anyNA(idx) && !all(is.na(idx))) {
    cli::cli_warn("Some labels are not in {.arg ordered_levels}; treated as missing.")
  }
  idx
}

# Paired sign test on the discordant items.
paired_flip_test <- function(before, after) {
  keep <- !is.na(before) & !is.na(after)
  before <- before[keep]
  after <- after[keep]
  d <- after - before
  improved <- sum(d > 0)
  worsened <- sum(d < 0)
  discordant <- improved + worsened
  flip_rate <- if (length(d) > 0) discordant / length(d) else NA_real_

  if (discordant == 0) {
    return(list(
      flip_rate = flip_rate, direction = "none",
      statistic = 0L, n_discordant = 0L, p_value = NA_real_
    ))
  }
  p <- stats::binom.test(improved, discordant, 0.5)$p.value
  direction <- if (improved > worsened) {
    "favors_perturbed"
  } else if (worsened > improved) {
    "favors_original"
  } else {
    "balanced"
  }
  list(
    flip_rate = flip_rate, direction = direction,
    statistic = improved, n_discordant = discordant, p_value = p
  )
}
