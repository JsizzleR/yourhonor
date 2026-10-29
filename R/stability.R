#' Test-retest stability of a judge
#'
#' Re-grades the same items and reports chance-corrected agreement plus mean
#' pairwise disagreement. `any_flip_rate_at_n_runs` is retained as a descriptive
#' quantity but must not be compared across different `n_runs`, because it rises
#' mechanically as more opportunities to flip are observed.
#'
#' @param data A data frame of items to grade.
#' @param judge A judge function `(data) -> labels`.
#' @param input_cols Columns visible to `judge`. Required so labels and other
#'   audit-only fields are not passed through accidentally.
#' @param id_col Optional unique source identifier. It is validated but is not
#'   exposed to the judge unless explicitly included in `input_cols`.
#' @param n_runs Number of repeated gradings; must be at least two.
#' @param seed Optional R random seed. This does not force deterministic behavior
#'   from a remote model provider.
#' @param label_levels Optional complete label universe. Declared levels no run
#'   used do not count toward Gwet's chance term; see [agreement()].
#' @param weights Agreement weights passed to Gwet's coefficient; see
#'   [agreement()] for the weight schemes.
#' @param conf_level Confidence level for both intervals.
#' @param n_boot Number of item-bootstrap resamples.
#' @param audit_id Optional evidence-bundle identifier. Use the same value as
#'   the related calibration and bias probes.
#'
#' @return A `yourhonor_stability` object with raw runs, per-item evidence,
#'   missingness, pairwise disagreement, Gwet agreement, and intervals.
#'   `stability_status` explains why chance-corrected agreement is unavailable;
#'   in particular, one observed category is `insufficient_variation`, not
#'   perfect AC1/AC2. The pairwise-disagreement interval remains available.
#'
#'   The pairwise-disagreement interval is a hybrid: the percentile item
#'   bootstrap widened to include a Wilson score interval for the mean
#'   per-item disagreement over `n_items_scored` items. The Wilson interval
#'   keeps a zero-disagreement sample from producing a zero-width interval and
#'   typically sets both bounds once `n_runs > 2`, so this interval is
#'   conservative relative to the bootstrap alone and widens only, never
#'   narrows. The chance-corrected interval is the plain item bootstrap.
#'
#'   `runs` holds labels in source-row order. Because every run is graded
#'   under a fresh shuffled key, `judge_item_ids` is an integer matrix parallel
#'   to `runs` whose entry `[i, r]` is the `.item_id` the judge saw for source
#'   row `i` in run `r`; join judge-side logs on that key, never on the row
#'   number.
#' @export
#' @examples
#' noisy_judge <- function(d) sample(c("C", "P"), nrow(d), replace = TRUE)
#' data <- tibble::tibble(input = paste("answer", 1:10))
#' test_retest(data, noisy_judge, input_cols = "input",
#'             n_runs = 4, n_boot = 100, seed = 1)
test_retest <- function(data,
                        judge,
                        input_cols = NULL,
                        id_col = NULL,
                        n_runs = 5,
                        seed = NULL,
                        label_levels = NULL,
                        weights = c("unweighted", "linear", "quadratic", "ordinal"),
                        conf_level = 0.95,
                        n_boot = 1000,
                        audit_id = NULL) {
  if (!is.function(judge)) {
    cli::cli_abort("{.arg judge} must be a function to re-run for stability.")
  }
  n_runs <- check_count(n_runs, minimum = 2L)
  n_boot <- check_count(n_boot)
  audit_id <- check_audit_id(audit_id)
  conf_level <- check_probability(conf_level, open = TRUE)
  weights <- rlang::arg_match(weights)
  if (weights != "unweighted" && is.null(label_levels)) {
    cli::cli_abort(
      "{.arg label_levels} is required for weighted stability; ordinal order cannot be inferred."
    )
  }
  data <- tibble::as_tibble(data)
  if (nrow(data) < 1L) cli::cli_abort("{.arg data} must contain at least one item.")
  if (!is.null(seed)) withr::local_seed(seed)
  judge_input <- redacted_judge_input(data, input_cols, id_col = id_col)

  graded <- lapply(seq_len(n_runs), function(i) {
    grade_blind(judge, judge_input, label_levels)
  })
  runs <- vapply(graded, function(g) g$label, character(nrow(data)))
  runs <- matrix(runs, nrow = nrow(data), ncol = n_runs)
  # The rating matrix is character, so a judge's factor levels would be lost
  # before `resolve_label_levels()` could use them; every run's declared
  # levels are kept, in first-seen order, followed by any label a run
  # returned outside them (the rule resolve_label_levels() applies to mixed
  # factor and character columns), so an inferred universe never rejects a
  # run's own output.
  if (is.null(label_levels)) {
    inferred <- unique(unlist(lapply(graded, `[[`, "label_levels")))
    if (length(inferred) > 0L) {
      observed <- unique(stats::na.omit(as.vector(runs)))
      label_levels <- unique(c(inferred, observed))
    }
  }
  judge_item_ids <- vapply(graded, function(g) {
    source_judge_item_ids(g$blinding_map, nrow(data))
  }, integer(nrow(data)))
  judge_item_ids <- matrix(judge_item_ids, nrow = nrow(data), ncol = n_runs)

  out <- summarize_stability_runs(
    runs = runs,
    label_levels = label_levels,
    weights = weights,
    conf_level = conf_level,
    n_boot = n_boot,
    arg = "judge",
    audit_id = audit_id
  )
  out$judge_item_ids <- judge_item_ids
  out
}

summarize_stability_runs <- function(runs,
                                     label_levels = NULL,
                                     weights = "unweighted",
                                     conf_level = 0.95,
                                     n_boot = 1000,
                                     audit_id = NULL,
                                     arg = "ratings",
                                     call = rlang::caller_env()) {
  runs <- as.matrix(runs)
  storage.mode(runs) <- "character"
  if (nrow(runs) < 1L || ncol(runs) < 2L) {
    cli::cli_abort(
      "Stability ratings need at least one item and two runs.",
      call = call
    )
  }
  n_runs <- ncol(runs)
  if (all(is.na(runs))) {
    cli::cli_abort("Every stability judgment was missing.", call = call)
  }
  label_levels <- resolve_label_levels(
    as.data.frame(runs, stringsAsFactors = FALSE),
    label_levels,
    allow_single = TRUE,
    arg = arg,
    call = call
  )

  observed_count <- apply(runs, 1, function(z) sum(!is.na(z)))
  per_item <- tibble::tibble(
    .row = seq_len(nrow(runs)),
    modal = apply(runs, 1, modal_value),
    n_observed = observed_count,
    n_missing = n_runs - observed_count,
    n_distinct = apply(runs, 1, function(z) length(unique(stats::na.omit(z)))),
    flipped = apply(runs, 1, function(z) {
      z <- stats::na.omit(z)
      if (length(z) < 2L) NA else length(unique(z)) > 1L
    }),
    tie = apply(runs, 1, is_tie)
  )

  item_disagreement <- item_pairwise_disagreement(runs)
  per_item$pairwise_disagreement <- item_disagreement
  pairwise_disagreement <- mean_or_na(item_disagreement)
  pairwise <- if (is.na(pairwise_disagreement)) NA_real_ else 1 - pairwise_disagreement
  chance_levels <- chance_categories(runs, label_levels)
  chance <- chance_corrected_stability(
    runs, weights, label_levels, chance_levels
  )
  boot <- bootstrap_stability(
    runs, item_disagreement, n_boot, conf_level, weights, label_levels,
    chance_levels
  )
  any_flip <- mean_or_na(per_item$flipped)
  stability_boundary_degenerate <- is_boundary_degenerate(
    chance$status, chance$estimate, boot$stability
  )
  pairwise_ci_reliable <- ci_is_reliable(boot$pairwise, n_boot)
  stability_ci_reliable <- ci_is_reliable(
    boot$stability, n_boot, stability_boundary_degenerate
  )

  structure(
    list(
      per_item = per_item,
      runs = runs,
      any_flip_rate_at_n_runs = any_flip,
      flip_rate = any_flip,
      pairwise_disagreement = pairwise_disagreement,
      pairwise_disagreement_conf_low = boot$pairwise$conf_low,
      pairwise_disagreement_conf_high = boot$pairwise$conf_high,
      stability = chance$estimate,
      stability_conf_low = boot$stability$conf_low,
      stability_conf_high = boot$stability$conf_high,
      stability_status = chance$status,
      stability_boundary_degenerate = stability_boundary_degenerate,
      percent_agreement = pairwise,
      missing_rate = mean(is.na(runs)),
      n_runs = n_runs,
      n_items = nrow(runs),
      n_items_scored = sum(is.finite(item_disagreement)),
      label_levels = label_levels,
      weights = weights,
      stability_coefficient = if (weights == "unweighted") "gwet_ac1" else "gwet_ac2",
      conf_level = conf_level,
      n_boot = n_boot,
      pairwise_n_boot_effective = boot$pairwise$n_effective,
      stability_n_boot_effective = boot$stability$n_effective,
      n_boot_effective = min(boot$pairwise$n_effective, boot$stability$n_effective),
      pairwise_ci_reliable = pairwise_ci_reliable,
      stability_ci_reliable = stability_ci_reliable,
      ci_reliable = pairwise_ci_reliable && stability_ci_reliable,
      audit_id = audit_id
    ),
    class = "yourhonor_stability"
  )
}

#' @export
print.yourhonor_stability <- function(x, ...) {
  cli::cli_h2("yourhonor test-retest stability")
  cli::cli_text(
    "{x$n_runs} runs - pairwise disagreement {fmt_num(x$pairwise_disagreement)} ",
    "[{fmt_num(x$pairwise_disagreement_conf_low)}, ",
    "{fmt_num(x$pairwise_disagreement_conf_high)}]."
  )
  if (identical(x$stability_status, "ok")) {
    cli::cli_text(
      "{x$stability_coefficient}: {fmt_num(x$stability)} ",
      "[{fmt_num(x$stability_conf_low)}, {fmt_num(x$stability_conf_high)}]."
    )
  } else {
    cli::cli_text(
      "{x$stability_coefficient}: not estimable ({x$stability_status})."
    )
  }
  cli::cli_text(
    "Any-flip rate at exactly {x$n_runs} runs: ",
    "{fmt_num(x$any_flip_rate_at_n_runs)}; missing judgments: {fmt_num(x$missing_rate)}."
  )
  invisible(x)
}

# Map each source row to the shuffled key the judge saw in one grading pass.
source_judge_item_ids <- function(blinding_map, n) {
  blinding_map$.item_id[match(seq_len(n), blinding_map$.source_row)]
}

mean_or_na <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

modal_value <- function(z) {
  z <- z[!is.na(z)]
  if (length(z) == 0L) return(NA_character_)
  tab <- table(z)
  top <- names(tab)[tab == max(tab)]
  if (length(top) > 1L) NA_character_ else top
}

is_tie <- function(z) {
  z <- z[!is.na(z)]
  if (length(z) == 0L) return(FALSE)
  tab <- table(z)
  sum(tab == max(tab)) > 1L
}

# Estimate and status are derived from the same filtered matrix so they can
# never disagree about why an estimate is unavailable.
chance_corrected_stability <- function(runs,
                                       weights,
                                       label_levels,
                                       chance_levels) {
  unavailable <- function(status) list(estimate = NA_real_, status = status)
  usable <- runs[rowSums(!is.na(runs)) >= 2L, , drop = FALSE]
  if (nrow(usable) < 2L) return(unavailable("insufficient_items"))
  # Every usable row has two observed runs, so at least two columns survive
  # this filter; it only removes all-missing runs that irrCAC cannot score.
  usable <- usable[, colSums(!is.na(usable)) > 0L, drop = FALSE]
  if (length(unique(stats::na.omit(as.vector(usable)))) < 2L) {
    return(unavailable("insufficient_variation"))
  }
  # The caller fixes `chance_levels` from the full data with
  # chance_categories(), the rule agreement() applies, so every bootstrap
  # draw is compared with the same estimand.
  estimate <- tryCatch(
    coeff_point(usable, "gwet_ac1", weights, label_levels, chance_levels),
    error = function(e) NA_real_
  )
  if (!is.finite(estimate)) return(unavailable("undefined"))
  list(estimate = estimate, status = "ok")
}

item_pairwise_disagreement <- function(runs) {
  apply(runs, 1, function(values) {
    values <- values[!is.na(values)]
    if (length(values) < 2L) return(NA_real_)
    pairs <- utils::combn(values, 2L)
    mean(pairs[1, ] != pairs[2, ])
  })
}

wilson_interval <- function(probability, n, conf_level) {
  if (!is.finite(probability) || n < 1L) return(c(NA_real_, NA_real_))
  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  z2 <- z^2
  denominator <- 1 + z2 / n
  center <- (probability + z2 / (2 * n)) / denominator
  half <- z * sqrt(probability * (1 - probability) / n + z2 / (4 * n^2)) /
    denominator
  pmax(0, pmin(1, c(center - half, center + half)))
}

# The pairwise statistic is the mean of a per-item vector, so the bootstrap
# resamples that vector directly; only the chance-corrected coefficient needs
# the resampled label matrix. Both use the same item draw so the two intervals
# describe the same resamples.
bootstrap_stability <- function(runs,
                                item_disagreement,
                                n_boot,
                                conf_level,
                                weights,
                                label_levels,
                                chance_levels = label_levels) {
  n <- nrow(runs)
  pairwise <- numeric(n_boot)
  stability <- numeric(n_boot)
  for (i in seq_len(n_boot)) {
    idx <- sample.int(n, n, replace = TRUE)
    pairwise[[i]] <- mean_or_na(item_disagreement[idx])
    stability[[i]] <- chance_corrected_stability(
      runs[idx, , drop = FALSE], weights, label_levels, chance_levels
    )$estimate
  }
  list(
    pairwise = widen_to_wilson(
      bootstrap_quantiles(pairwise, conf_level), item_disagreement, conf_level
    ),
    stability = bootstrap_quantiles(stability, conf_level)
  )
}

# A sample with no observed disagreement bootstraps to a zero-width interval at
# 0, which overstates certainty for small item counts. The Wilson score
# interval for the mean per-item disagreement has positive width there, so the
# reported interval is the union of the two: it can only widen the bootstrap.
widen_to_wilson <- function(pairwise_ci, item_disagreement, conf_level) {
  scored <- item_disagreement[is.finite(item_disagreement)]
  wilson_ci <- wilson_interval(
    mean_or_na(scored), length(scored), conf_level
  )
  if (all(is.finite(wilson_ci))) {
    pairwise_ci$conf_low <- min(
      pairwise_ci$conf_low, wilson_ci[[1]], na.rm = TRUE
    )
    pairwise_ci$conf_high <- max(
      pairwise_ci$conf_high, wilson_ci[[2]], na.rm = TRUE
    )
  }
  pairwise_ci
}
