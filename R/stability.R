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
#' @param label_levels Optional complete label universe. Unweighted, a
#'   declared level no run used changes no coefficient. Weighted, the weights
#'   come from the full declared scale, so an unused level can still change
#'   AC2; see [agreement()].
#' @param weights Agreement weights passed to Gwet's coefficient; see
#'   [agreement()] for the weight schemes.
#' @param conf_level Confidence level for every interval.
#' @param n_boot Number of item-bootstrap resamples.
#' @param audit_id Optional evidence-bundle identifier. Use the same value as
#'   the related calibration and bias probes.
#'
#' @return A `yourhonor_stability` object with raw runs, per-item evidence,
#'   missingness, pairwise disagreement, Gwet agreement, and intervals.
#'   `stability_status` explains why chance-corrected agreement is unavailable;
#'   in particular, one observed category is `insufficient_variation`, not
#'   perfect AC1/AC2, and `stability_conf_low` and `stability_conf_high`
#'   are then `NA`. The pairwise-disagreement interval remains available.
#'
#'   The pairwise-disagreement interval is a hybrid: the percentile item
#'   bootstrap widened to include a Wilson score interval for the mean
#'   per-item disagreement over `n_items_scored` items. The Wilson interval
#'   keeps a zero-disagreement sample from producing a zero-width interval and
#'   typically sets both bounds once `n_runs > 2`, so this interval is
#'   conservative relative to the bootstrap alone and widens only, never
#'   narrows. The chance-corrected interval is the plain item bootstrap.
#'
#'   Missing judgments are left out of the observed statistics, so a judge
#'   that abstains where it would have been unstable looks stable there. The
#'   worst-case fields bound that: `pairwise_disagreement_worst_case` is the
#'   mean over all `n_items` items of a per-item disagreement in which every
#'   pair involving a missing judgment disagrees (an item with fewer than two
#'   judgments counts as 1; `per_item$pairwise_disagreement_worst_case` holds
#'   the per-item values), and `pairwise_disagreement_worst_case_high` is its
#'   upper bound, computed like the observed one on the same bootstrap draws
#'   and never below `pairwise_disagreement_conf_high`. With no missing
#'   judgment it equals `pairwise_disagreement_conf_high`, and it is `NA`
#'   when that is (no item has two judgments). This is the upper bound
#'   [acceptance_policy()] assesses.
#'
#'   Missing judgments can also raise the observed disagreement, when the
#'   runs a judge left out are ones that agreed. The best-case fields bound
#'   that side: `pairwise_disagreement_best_case` is the mean over all
#'   `n_items` items of the smallest per-item disagreement any labels for the
#'   missing judgments could give, which each missing judgment's taking its
#'   item's most common observed label achieves (an item with fewer than two
#'   judgments counts as 0; `per_item$pairwise_disagreement_best_case` holds
#'   the per-item values), and `pairwise_disagreement_best_case_low` is its
#'   lower bound, computed like the observed one on the same bootstrap draws
#'   (widened to the Wilson interval over `n_items`) and never above
#'   `pairwise_disagreement_conf_low`. With no missing judgment it equals
#'   `pairwise_disagreement_conf_low`, and it is `NA` when that is. This is
#'   the lower bound [acceptance_policy()] assesses, so a `FAIL` holds under
#'   the most favourable labels for the missing judgments. A numeric `NaN`
#'   judgment is missing, not a label.
#'
#'   `any_flip_rate_at_n_runs` is over the `n_items_complete` items with
#'   every run observed (`NA` when there are none), since an incomplete item
#'   had fewer chances to flip; under selective abstention it describes
#'   complete items only. `any_flip_rate_worst_case` counts every item with
#'   an observed flip or a missing judgment as flipped, over all `n_items`.
#'
#'   `bootstrap_seed` is the random-number state (`.Random.seed`) the item
#'   bootstrap started from, or `NULL` for a user-supplied generator.
#'   [acceptance_policy()] does not trust the stored bounds: it replays the
#'   bootstrap from `runs`, `n_boot`, `conf_level` and this state, rebuilds
#'   the four pairwise-disagreement bounds,
#'   `pairwise_ci_reliable`, `n_items`, `n_runs`, `n_items_scored` and
#'   `missing_rate`, and assesses the rebuilt values. Evidence whose stored
#'   values do not match the rebuild, or that cannot be rebuilt (for
#'   example no run matrix, or no state with `n_boot` above 0, as from a
#'   user-supplied generator, whose evidence therefore always reads
#'   `INDETERMINATE`), makes every stability check `INDETERMINATE`, so an
#'   edited bound can neither pass nor fail a judge.
#'   Replaying draws nothing from the caller's random-number stream.
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
    yh_abort("{.arg judge} must be a function to re-run for stability.")
  }
  n_runs <- check_count(n_runs, minimum = 2L)
  n_boot <- check_count(n_boot)
  audit_id <- check_audit_id(audit_id)
  conf_level <- check_probability(conf_level, open = TRUE)
  seed <- check_seed(seed)
  weights <- rlang::arg_match(weights)
  if (weights != "unweighted" && is.null(label_levels)) {
    yh_abort(
      "{.arg label_levels} is required for weighted stability; ordinal order cannot be inferred."
    )
  }
  data <- tibble::as_tibble(data)
  if (nrow(data) < 1L) yh_abort("{.arg data} must contain at least one item.")
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
    yh_abort(
      "Stability ratings need at least one item and two runs.",
      call = call
    )
  }
  n_runs <- ncol(runs)
  if (all(is.na(runs))) {
    yh_abort("Every stability judgment was missing.", call = call)
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
  worst_case <- item_worst_case_disagreement(runs, item_disagreement)
  best_case <- item_best_case_disagreement(runs)
  per_item$pairwise_disagreement <- item_disagreement
  per_item$pairwise_disagreement_worst_case <- worst_case
  per_item$pairwise_disagreement_best_case <- best_case
  pairwise_disagreement <- mean_or_na(item_disagreement)
  pairwise <- if (is.na(pairwise_disagreement)) NA_real_ else 1 - pairwise_disagreement
  chance_levels <- chance_categories(runs, label_levels)
  chance <- chance_corrected_stability(
    runs, weights, label_levels, chance_levels
  )
  bootstrap_seed <- bootstrap_rng_state(n_boot)
  boot <- bootstrap_stability(
    runs, item_disagreement, n_boot, conf_level, weights, label_levels,
    chance_levels, worst_case = worst_case, best_case = best_case
  )
  # Draws that happened to resample enough usable items do not give an
  # interval for an estimate the full sample could not produce.
  if (!identical(chance$status, "ok")) {
    boot$stability$conf_low <- NA_real_
    boot$stability$conf_high <- NA_real_
    boot$stability$ci_reliable <- FALSE
  }
  case_bounds <- assessed_case_bounds(boot)
  worst_case_high <- case_bounds$worst_case_high
  best_case_low <- case_bounds$best_case_low
  # An item's any-flip status at `n_runs` is known only when every run was
  # observed (a flip already seen is permanent, but an unflipped incomplete
  # item could still flip), so the rate is over complete items and the
  # worst case counts every incomplete, unflipped item as a flip.
  complete <- observed_count == n_runs
  known_flip <- per_item$n_distinct >= 2L
  any_flip <- if (any(complete)) mean(known_flip[complete]) else NA_real_
  any_flip_worst_case <- mean(known_flip | !complete)
  stability_boundary_degenerate <- is_boundary_degenerate(
    chance$status, chance$estimate, boot$stability
  )
  pairwise_ci_reliable <- ci_is_reliable(boot$pairwise, n_boot)
  stability_ci_reliable <- ci_is_reliable(
    boot$stability, n_boot, stability_boundary_degenerate
  )

  out <- new_yourhonor_stability(list(
    per_item = per_item,
    runs = runs,
    any_flip_rate_at_n_runs = any_flip,
    any_flip_rate_worst_case = any_flip_worst_case,
    pairwise_disagreement = pairwise_disagreement,
    pairwise_disagreement_conf_low = boot$pairwise$conf_low,
    pairwise_disagreement_conf_high = boot$pairwise$conf_high,
    pairwise_disagreement_worst_case = mean(worst_case),
    pairwise_disagreement_worst_case_high = worst_case_high,
    pairwise_disagreement_best_case = mean(best_case),
    pairwise_disagreement_best_case_low = best_case_low,
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
    n_items_complete = sum(complete),
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
  ))
  # Appended after the schema, like `judge_item_ids`, so an object from
  # before it existed still reaches the policy, which can rebuild it only
  # when `n_boot` is 0 (verify_stability_evidence()).
  out["bootstrap_seed"] <- list(bootstrap_seed)
  out
}

# The random-number state the item bootstrap starts from, so the policy can
# redraw the same items (verify_stability_evidence()). Reading it draws
# nothing. With no state yet, R would seed itself from the clock at the first
# draw; set.seed(NULL) does the same one step earlier, so the state is the one
# the draws use. NULL when there are no draws, and for a user-supplied
# generator, whose state need not live in `.Random.seed`.
bootstrap_rng_state <- function(n_boot) {
  if (n_boot < 1L) return(NULL)
  if (!exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    set.seed(NULL)
  }
  if (identical(RNGkind()[[1]], "user-supplied")) return(NULL)
  get(".Random.seed", envir = globalenv(), inherits = FALSE)
}

# The two case bounds the policy assesses, from bootstrap_stability()'s
# intervals: each is combined with the observed bound so it can only widen
# it, and is NA when the observed bound is.
assessed_case_bounds <- function(boot) {
  list(
    worst_case_high = if (is.finite(boot$pairwise$conf_high)) {
      max(boot$pairwise$conf_high, boot$worst_case$conf_high, na.rm = TRUE)
    } else {
      NA_real_
    },
    best_case_low = if (is.finite(boot$pairwise$conf_low)) {
      min(boot$pairwise$conf_low, boot$best_case$conf_low, na.rm = TRUE)
    } else {
      NA_real_
    }
  )
}

# Every field of a stability object that the policy reads and its runs
# determine. verify_stability_evidence() rebuilds each one.
stability_rebuilt_fields <- c(
  "pairwise_disagreement_conf_low", "pairwise_disagreement_conf_high",
  "pairwise_disagreement_worst_case_high",
  "pairwise_disagreement_best_case_low", "pairwise_ci_reliable",
  "n_items", "n_runs", "n_items_scored", "missing_rate"
)

# Doubles rebuilt on another platform can differ in their last bits (mean()
# accumulates in long double where there is one). The policy assesses the
# rebuilt values, so this allowance gives an edit no room.
stability_rebuild_tolerance <- 1e-9

# A stability object's bounds and counts rebuilt from its own `runs`,
# `n_boot`, `conf_level` and `bootstrap_seed`, with the draws
# summarize_stability_runs() made: each bootstrap iteration is one
# sample.int(n, n, replace = TRUE), and nothing else draws, so restoring the
# state replays them. Every stored field in stability_rebuilt_fields must
# match its rebuild; the object is returned with the rebuilt values in their
# place (bit for bit the stored ones on the platform that made it). Evidence
# that cannot be rebuilt, or whose stored fields do not match, gives
# `reason` instead, so a hand-edited bound can neither PASS nor FAIL.
verify_stability_evidence <- function(stability) {
  # Only an edited object can make the rebuild raise an error (a label R
  # cannot translate, a state R refuses); it then cannot be rebuilt, never an
  # error out of the policy. A warning is not caught: one raised while a
  # genuine rebuild runs (the byte compiler's, say) says nothing about the
  # evidence. An object of any other class is refused, since writing the
  # rebuilt fields back would dispatch on it.
  rebuilt <- if (identical(class(stability), "yourhonor_stability")) {
    tryCatch(rebuild_stability_fields(stability), error = function(e) NULL)
  }
  if (is.null(rebuilt)) {
    return(list(
      stability = NULL,
      reason = "Stability evidence cannot be rebuilt from its runs."
    ))
  }
  matches <- vapply(stability_rebuilt_fields, function(field) {
    stored_matches(stability[[field]], rebuilt[[field]])
  }, logical(1))
  if (!all(matches)) {
    return(list(
      stability = NULL,
      reason = paste(
        "Stored stability bounds or counts do not match their rebuild from",
        "its runs."
      )
    ))
  }
  for (field in stability_rebuilt_fields) {
    stability[[field]] <- rebuilt[[field]]
  }
  list(stability = stability, reason = NULL)
}

# A stored field matches its rebuild when both are unavailable, or both are
# one number within the tolerance (one logical for the reliability flag).
stored_matches <- function(stored, rebuilt) {
  # A genuine field is never classed, and a class's methods would dispatch.
  if (is.object(stored)) return(FALSE)
  if (is.logical(rebuilt)) return(identical(stored, rebuilt))
  if (!is.numeric(stored) || length(stored) != 1L) return(FALSE)
  if (is.na(rebuilt) || is.na(stored)) {
    return(is.na(rebuilt) && is.na(stored) && !is.nan(stored))
  }
  is.finite(stored) &&
    abs(as.numeric(stored) - rebuilt) <= stability_rebuild_tolerance
}

# NULL when the inputs are not what summarize_stability_runs() writes: a
# character matrix of at least one item and two runs with a judgment, a
# whole n_boot in the integer range, a conf_level in (0, 1), and a state for
# a positive n_boot: an unclassed integer vector of a kind code (its first
# element, never NA) and at least one word, as every generator R replays
# keeps (a lone kind code R would reseed from the clock). A later word may
# read as NA, since -2^31 is NA_integer_ in R.
rebuild_stability_fields <- function(stability) {
  runs <- stability[["runs"]]
  n_boot <- stability[["n_boot"]]
  conf_level <- stability[["conf_level"]]
  state <- stability[["bootstrap_seed"]]
  # A classed input is refused before anything reads it, since its methods
  # (dim(), ==, as.integer()) would dispatch.
  if (any(vapply(list(runs, n_boot, conf_level, state), is.object,
                 logical(1)))) {
    return(NULL)
  }
  # More runs than give an integer count of pairs cannot be scored (nor
  # summarized in the first place).
  usable <- is.matrix(runs) && is.character(runs) && nrow(runs) >= 1L &&
    ncol(runs) >= 2L && choose(ncol(runs), 2) <= .Machine$integer.max &&
    !all(is.na(runs)) &&
    is_finite_number(n_boot) && n_boot >= 0 && n_boot == round(n_boot) &&
    n_boot <= .Machine$integer.max &&
    is_finite_number(conf_level) && conf_level > 0 && conf_level < 1
  if (!usable) return(NULL)
  n_boot <- as.integer(n_boot)
  if (n_boot >= 1L &&
      !(is.integer(state) && length(state) >= 2L && !is.na(state[[1L]]))) {
    return(NULL)
  }
  item_disagreement <- item_pairwise_disagreement(runs)
  worst_case <- item_worst_case_disagreement(runs, item_disagreement)
  best_case <- item_best_case_disagreement(runs)
  # A state R refuses (a wrong length, an unknown kind) errors, and
  # verify_stability_evidence() reads that as evidence it cannot rebuild.
  boot <- with_rng_state(state, pairwise_bootstrap(
    item_disagreement, worst_case, best_case, n_boot, conf_level
  ))
  case_bounds <- assessed_case_bounds(boot)
  list(
    pairwise_disagreement_conf_low = boot$pairwise$conf_low,
    pairwise_disagreement_conf_high = boot$pairwise$conf_high,
    pairwise_disagreement_worst_case_high = case_bounds$worst_case_high,
    pairwise_disagreement_best_case_low = case_bounds$best_case_low,
    pairwise_ci_reliable = ci_is_reliable(boot$pairwise, n_boot),
    n_items = nrow(runs),
    n_runs = ncol(runs),
    n_items_scored = sum(is.finite(item_disagreement)),
    missing_rate = mean(is.na(runs))
  )
}

# Runs `code` from the random-number state `state` (none when NULL) and puts
# the caller's state back afterwards, its generator kinds included, so an
# assessment leaves the caller's stream where it was. With no state before,
# RNGkind() resets the kinds (it writes a state, which is then removed).
with_rng_state <- function(state, code) {
  if (is.null(state)) return(code)
  kinds <- RNGkind()
  had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old <- if (had) get(".Random.seed", envir = globalenv(), inherits = FALSE)
  on.exit({
    if (had) {
      assign(".Random.seed", old, envir = globalenv())
    } else {
      # The replayed state goes first: RNGkind() reads the state, and one R
      # refuses would make it error and stay behind.
      rm_rng_state()
      suppressWarnings(RNGkind(kinds[[1]], kinds[[2]], kinds[[3]]))
      rm_rng_state()
    }
  }, add = TRUE)
  assign(".Random.seed", state, envir = globalenv())
  code
}

rm_rng_state <- function() {
  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    rm(".Random.seed", envir = globalenv())
  }
}

# The three pairwise intervals of bootstrap_stability() without the
# chance-corrected coefficient, from the same draws: the item indices are
# drawn in the same order, one sample.int(n, n, replace = TRUE) per
# iteration, a chunk at a time.
pairwise_bootstrap <- function(item_disagreement,
                               worst_case,
                               best_case,
                               n_boot,
                               conf_level,
                               chunk_cells = bootstrap_chunk_cells) {
  n <- length(item_disagreement)
  pairwise <- numeric(n_boot)
  worst <- numeric(n_boot)
  best <- numeric(n_boot)
  if (n_boot >= 1L) {
    chunk <- max(1L, min(n_boot, chunk_cells %/% n))
    for (first in seq(1L, n_boot, by = chunk)) {
      iters <- first:min(n_boot, first + chunk - 1L)
      idx <- draw_item_indices(n, length(iters))
      pairwise[iters] <- resample_means(item_disagreement, idx)
      worst[iters] <- resample_means(worst_case, idx)
      best[iters] <- resample_means(best_case, idx)
    }
  }
  pairwise_intervals(
    pairwise, worst, best, item_disagreement, worst_case, best_case,
    conf_level
  )
}

# One column of item indices per bootstrap iteration.
draw_item_indices <- function(n, n_iter) {
  idx <- matrix(0L, n, n_iter)
  for (j in seq_len(n_iter)) idx[, j] <- sample.int(n, n, replace = TRUE)
  idx
}

resample_means <- function(x, idx) {
  vapply(seq_len(ncol(idx)), function(j) mean_or_na(x[idx[, j]]), numeric(1))
}

pairwise_intervals <- function(pairwise, worst, best, item_disagreement,
                               worst_case, best_case, conf_level) {
  list(
    pairwise = widen_to_wilson(
      bootstrap_quantiles(pairwise, conf_level), item_disagreement, conf_level
    ),
    worst_case = widen_to_wilson(
      bootstrap_quantiles(worst, conf_level), worst_case, conf_level
    ),
    best_case = widen_to_wilson(
      bootstrap_quantiles(best, conf_level), best_case, conf_level
    )
  )
}

# Every field of a yourhonor_stability, in order. new_yourhonor_stability()
# refuses any other set, and report() and write_audit() validate supplied
# stability evidence against these names, so the constructor and the
# validation cannot drift apart. Entry points may append fields after them
# (`judge_item_ids`, `provenance`).
stability_fields <- c(
  "per_item", "runs", "any_flip_rate_at_n_runs", "any_flip_rate_worst_case",
  "pairwise_disagreement", "pairwise_disagreement_conf_low",
  "pairwise_disagreement_conf_high", "pairwise_disagreement_worst_case",
  "pairwise_disagreement_worst_case_high", "pairwise_disagreement_best_case",
  "pairwise_disagreement_best_case_low", "stability", "stability_conf_low",
  "stability_conf_high", "stability_status", "stability_boundary_degenerate",
  "percent_agreement", "missing_rate", "n_runs", "n_items", "n_items_scored",
  "n_items_complete", "label_levels", "weights", "stability_coefficient",
  "conf_level", "n_boot", "pairwise_n_boot_effective",
  "stability_n_boot_effective", "n_boot_effective", "pairwise_ci_reliable",
  "stability_ci_reliable", "ci_reliable", "audit_id"
)

new_yourhonor_stability <- function(fields) {
  if (!identical(names(fields), stability_fields)) {
    cli::cli_abort("Internal error: stability fields do not match the schema.")
  }
  structure(fields, class = "yourhonor_stability")
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
  # Exact lookups, each a finite scalar: `$` would partially match an absent
  # field to another, and an object without these fields predates them.
  worst_point <- x[["pairwise_disagreement_worst_case"]]
  worst_high <- x[["pairwise_disagreement_worst_case_high"]]
  worst_flip <- x[["any_flip_rate_worst_case"]]
  if (isTRUE(x$missing_rate > 0) && is_finite_number(worst_point) &&
      is_finite_number(worst_high) && is_finite_number(worst_flip)) {
    cli::cli_text(
      "Worst case, counting missing judgments as disagreements: pairwise ",
      "disagreement {fmt_num(worst_point)} (upper {fmt_num(worst_high)}); ",
      "any-flip rate {fmt_num(worst_flip)}."
    )
  }
  best_point <- x[["pairwise_disagreement_best_case"]]
  best_low <- x[["pairwise_disagreement_best_case_low"]]
  if (isTRUE(x$missing_rate > 0) && is_finite_number(best_point) &&
      is_probability_bound(best_low)) {
    cli::cli_text(
      "Best case, giving each missing judgment the label that least ",
      "disagrees with its item's other runs: pairwise disagreement ",
      "{fmt_num(best_point)} (lower {fmt_num(best_low)})."
    )
  }
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

# The largest per-item disagreement any labels for the missing judgments
# could produce: every pair that involves a missing judgment counts as a
# disagreement, over all `choose(n_runs, 2)` pairs. It does not use the label
# universe (with two labels three runs cannot all disagree), because an
# inferred universe holds only observed labels and a judge may abstain
# exactly where it would have used another. An item with fewer than two
# judgments is 1; a complete item keeps its observed value.
item_worst_case_disagreement <- function(runs, item_disagreement) {
  n_pairs <- choose(ncol(runs), 2L)
  observed_pairs <- choose(rowSums(!is.na(runs)), 2L)
  incomplete <- observed_pairs < n_pairs
  disagreeing <- ifelse(
    observed_pairs > 0, item_disagreement * observed_pairs, 0
  )
  worst_case <- item_disagreement
  worst_case[incomplete] <- (
    disagreeing[incomplete] + n_pairs - observed_pairs[incomplete]
  ) / n_pairs
  worst_case
}

# The smallest per-item disagreement any labels for the missing judgments
# could produce: each takes its item's most common observed label (any one of
# tied modes), and an item with no judgment takes one label throughout, so
# an item with fewer than two judgments is 0. Brute force over every
# completion, unseen labels included, gives the same minimum. The filled
# matrix is scored by item_pairwise_disagreement() itself, so a complete item
# keeps its observed value bit for bit and an incomplete one gets exactly the
# double its modal completion would, never a rounding above it.
item_best_case_disagreement <- function(runs) {
  filled <- t(apply(runs, 1, function(z) {
    observed <- z[!is.na(z)]
    if (length(observed) == length(z)) return(z)
    # Only equality between labels matters, so any one label will do.
    mode <- if (length(observed) == 0L) {
      ""
    } else {
      tab <- table(observed)
      names(tab)[which.max(tab)]
    }
    z[is.na(z)] <- mode
    z
  }))
  dim(filled) <- dim(runs)
  dimnames(filled) <- dimnames(runs)
  item_pairwise_disagreement(filled)
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
# the resampled label matrix. All four use the same item draw so the
# intervals describe the same resamples; each worst-case draw is at least,
# and each best-case draw at most, the draw of every completion of the
# missing judgments.
#
# The draws are those of one sample.int(n, n, replace = TRUE) per iteration,
# drawn and scored a chunk at a time as bootstrap_coefficients() does, so the
# index and count matrices never exceed one chunk (bootstrap_chunk_cells, or
# one resample when n is larger) however large n_boot is. The chance-corrected
# draw is scored natively (stability_resample_values()), with
# chance_corrected_stability() as its oracle.
bootstrap_stability <- function(runs,
                                item_disagreement,
                                n_boot,
                                conf_level,
                                weights,
                                label_levels,
                                chance_levels = label_levels,
                                worst_case = item_disagreement,
                                best_case = item_disagreement,
                                chunk_cells = bootstrap_chunk_cells) {
  n <- nrow(runs)
  pairwise <- numeric(n_boot)
  worst <- numeric(n_boot)
  best <- numeric(n_boot)
  stability <- numeric(n_boot)
  if (n_boot >= 1L) {
    kernel <- stability_kernel(runs, weights, label_levels, chance_levels)
    chunk <- max(1L, min(n_boot, chunk_cells %/% n))
    # pairwise_bootstrap() replays these draws for the policy, so the two
    # share the draw, the resample mean and the intervals.
    for (first in seq(1L, n_boot, by = chunk)) {
      iters <- first:min(n_boot, first + chunk - 1L)
      idx <- draw_item_indices(n, length(iters))
      pairwise[iters] <- resample_means(item_disagreement, idx)
      worst[iters] <- resample_means(worst_case, idx)
      best[iters] <- resample_means(best_case, idx)
      stability[iters] <- stability_resample_values(
        kernel, runs, idx, weights, label_levels, chance_levels
      )
    }
  }
  c(
    pairwise_intervals(
      pairwise, worst, best, item_disagreement, worst_case, best_case,
      conf_level
    ),
    list(stability = bootstrap_quantiles(stability, conf_level))
  )
}

# The native terms of chance_corrected_stability() over the items it can
# score: those with at least two judgments, every one in a chance category
# (chance_categories() is taken over exactly those items). NULL when no
# resample can be scored: no such item, or fewer than two chance categories,
# so every resample observes at most one.
stability_kernel <- function(runs, weights, label_levels, chance_levels) {
  usable <- which(rowSums(!is.na(runs)) >= 2L)
  if (length(usable) == 0L || length(chance_levels) < 2L) return(NULL)
  kernel <- agreement_kernel(
    runs[usable, , drop = FALSE], weights, label_levels, chance_levels
  )
  kernel$usable <- usable
  kernel
}

# Each resample's chance-corrected value, given its item indices into `runs`
# as the columns of `idx`: what chance_corrected_stability() gives on
# runs[idx[, j], ]. A resample keeps only its items with two judgments, so its
# counts are those items' counts; one with fewer than two such items is
# insufficient_items and one that observes a single category is
# insufficient_variation, both NA. Every other value is native unless
# agreement_kernel_draws() cannot vouch for it, and chance_corrected_stability()
# (irrCAC) then scores that resample from the rows, in the order, it would
# have been given.
stability_resample_values <- function(kernel,
                                      runs,
                                      idx,
                                      weights,
                                      label_levels,
                                      chance_levels) {
  if (is.null(kernel)) return(rep(NA_real_, ncol(idx)))
  counts <- apply(idx, 2L, tabulate, nbins = nrow(runs))
  dim(counts) <- dim(idx)
  counts <- counts[kernel$usable, , drop = FALSE]
  native <- agreement_kernel_draws(kernel, counts, "gwet_ac1")$gwet_ac1
  value <- native$value
  oracle <- native$oracle
  few <- colSums(counts) < 2
  value[few] <- NA_real_
  oracle[few] <- FALSE
  for (j in which(oracle)) {
    value[[j]] <- chance_corrected_stability(
      runs[idx[, j], , drop = FALSE], weights, label_levels, chance_levels
    )$estimate
  }
  value
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
