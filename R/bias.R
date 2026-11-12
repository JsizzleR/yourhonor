#' Probe a pointwise judge for paired bias
#'
#' Grades each item under a control and a perturbed treatment condition in one
#' randomized batch and measures directional label movement. The verbosity
#' probe deliberately lengthens the treatment; the self-preference and
#' bandwagon probes change only a framing prefix. Position bias is
#' intentionally handled by [probe_position()], which uses structured
#' candidates and stable identities.
#'
#' @param data A data frame with a character `input` column. Missing inputs are
#'   an error: a pasted `NA` would otherwise be graded as a real answer and hide
#'   abstention from the missingness gate.
#' @param judge A judge function.
#' @param input_cols Columns visible to `judge`. The default exposes only
#'   `input`; all other columns are redacted.
#' @param id_col Optional unique source identifier. It is validated but is not
#'   exposed to the judge unless explicitly included in `input_cols`.
#' @param probes Any of `"verbosity"`, `"self_preference"`, and `"bandwagon"`.
#' @param ordered_levels Label scale from worst to best.
#' @param n Optional positive number of sampled items.
#' @param seed Optional R random seed.
#' @param flip_threshold Practical-significance floor for a `"BIAS"` verdict:
#'   the net `directional_rate` (see *Result columns*) must exceed it in
#'   addition to `p_value_adj < alpha`. A large but balanced `flip_rate` is
#'   reported yet never flagged.
#' @param alpha Significance level.
#' @param p_adjust Multiple-comparison correction applied across the requested
#'   probes. Probes with no label movement enter the family with `p_value = 1`;
#'   only `"insufficient_data"` probes are excluded.
#' @param conf_level Confidence level for the exact flip-rate interval.
#' @param audit_id Optional evidence-bundle identifier. Use the same value as
#'   the related calibration and stability run.
#'
#' @section Result columns:
#' [probe_bias()], [probe_position()], and [combine_bias()] return one row per
#' probe with the same columns. "Items" are rows of `data`; "judgments" are
#' individual judge outputs. Columns that apply only to the position probe are
#' `NA` for pointwise probes.
#'
#' * `probe`: probe name.
#' * `flip_rate`: any-direction change rate, `n_discordant / n_scored`, with
#'   its exact (Clopper-Pearson) interval `conf_low`/`conf_high` at
#'   `conf_level`. `NA` when nothing was scored.
#' * `direction`: pointwise `"favors_perturbed"`, `"favors_original"`,
#'   `"balanced"`, or `"none"` (no movement); position `"favors_first"`,
#'   `"favors_second"`, or `"balanced"`.
#' * `directional_rate`: net movement over scored items. Pointwise:
#'   `|improved - worsened| / n_scored`. Position: `|first - second| /
#'   n_scored`, where `first`/`second` count items whose canonical choice
#'   moved with the display order towards one slot. `NA` when nothing was
#'   scored. `directional_conf_low`/`directional_conf_high` is a Bonferroni
#'   combination of exact marginal intervals for that difference
#'   (conservative). Its upper bound is capped at `conf_high`, because the
#'   net rate can never exceed the any-direction flip rate over the same
#'   denominator.
#' * `position_preference`, `position_conf_low`, `position_conf_high`:
#'   position only. Share of directional items favouring the first display
#'   slot (`statistic / n_directional_items`) with its exact interval.
#' * `display_first_rate`: position only. Mean per-item share of decided
#'   judgments that chose the first display slot.
#' * `statistic`: sign-test successes (`improved` pointwise; items favouring
#'   the first slot for position).
#' * `n_discordant`: scored items whose label (pointwise) or canonical choice
#'   (position) changed between conditions.
#' * `n_scored`: items with a decided judgment in both conditions. For the
#'   position probe a condition's judgment is the mode across `repeats`, so a
#'   within-condition tie leaves the item unscored.
#' * `n_directional_items`: items entering the sign test. Equals
#'   `n_discordant` for both probes: a position item is directional only when
#'   it was decided in both display orders and its canonical choice moved
#'   with the order (a tie commits to neither slot).
#' * `n_input`: items supplied, after any sampling.
#' * `n_unscored`, `unscored_rate`: `n_input - n_scored` and its share of
#'   `n_input`. Includes position items lost to within-condition ties, so it
#'   can exceed `n_items_missing`.
#' * `n_unscored_tied`: position items lost to a within-condition tie that
#'   no answer could break: a display order judged in every repeat whose
#'   modal choice is tied, whatever the other order's missing judgments.
#'   Always 0 for pointwise probes, where an item is unscored only through a
#'   missing judgment.
#' * `n_items_missing`: items with at least one `NA` judgment.
#' * `n_judgments`, `n_judgments_missing`, `missing_rate`: individual judge
#'   outputs, how many were `NA`, and their share. `missing_rate` is gated
#'   directly by [acceptance_policy()]'s `max_probe_missing_rate`; unscored
#'   items also enter the policy's flip-rate and directional bounds over
#'   `n_input`, against the judge in the upper bounds and in its favour in
#'   the lower bounds (which leave out the `n_unscored_tied` items, so their
#'   denominator is `n_input - n_unscored_tied`), so the intervals a policy
#'   assesses can be wider than this row's `conf_low`-`conf_high` and
#'   `directional_conf_low`-`directional_conf_high`, which describe the
#'   scored items. So do scored position items whose missing repeats could
#'   change their outcome (see [acceptance_policy()]'s
#'   `max_probe_flip_rate`).
#' * `n_comparisons`: condition pairs judged (`n_input` pointwise;
#'   `n_input * repeats` position).
#' * `p_value`: exact sign-test p-value over `n_directional_items`; `1` when
#'   no item moved, `NA` only when `status` is `"insufficient_data"`.
#' * `p_value_adj`: `p_value` corrected with `p_adjust_method` across the rows
#'   of the object; `NA` p-values are excluded from the family.
#' * `verdict`: `"BIAS"` when `p_value_adj < alpha` and `directional_rate`
#'   exceeds `flip_threshold` (the position probe has no effect floor, shown
#'   as `flip_threshold = NA`); `"no_signal"` otherwise; `"INCONCLUSIVE"` when
#'   nothing was scored.
#' * `status`: `"ok"` or `"insufficient_data"` (`n_scored == 0`).
#' * `conf_level`, `alpha`, `flip_threshold`, `p_adjust_method`: the settings
#'   in force for that row.
#'
#' @return A `yourhonor_bias` tibble with one row per probe (see *Result
#'   columns*) and the attributes `audit_id`, `evidence`, and `item_evidence`.
#'   The two evidence attributes are lists named by probe; pointwise probes
#'   contribute no entries. `"no_signal"` means the probe did not detect
#'   directional bias; it is not proof that the judge is unbiased.
#' @export
#' @examples
#' length_judge <- function(d) ifelse(nchar(d$input) > 40, "C", "P")
#' data <- tibble::tibble(input = paste("answer", 1:20))
#' probe_bias(data, length_judge, probes = "verbosity", seed = 1)
probe_bias <- function(data,
                       judge,
                       input_cols = "input",
                       id_col = NULL,
                       probes = c("verbosity", "self_preference", "bandwagon"),
                       ordered_levels = c("I", "P", "C"),
                       n = NULL,
                       seed = NULL,
                       flip_threshold = 0.1,
                       alpha = 0.05,
                       p_adjust = c("holm", "none", "BH", "bonferroni"),
                       conf_level = 0.95,
                       audit_id = NULL) {
  if (!is.function(judge)) {
    yh_abort("{.arg judge} must be a function; probes re-grade perturbed inputs.")
  }
  audit_id <- check_audit_id(audit_id)
  if (!is.character(probes) || length(probes) < 1L || anyNA(probes) ||
      anyDuplicated(probes)) {
    yh_abort("{.arg probes} must contain one or more unique probe names.")
  }
  if (any(probes == "position")) {
    yh_abort(paste0(
      "Regex position probing was removed because it could not preserve candidate identity. ",
      "Use {.fn probe_position} with structured candidate columns."
    ))
  }
  probes <- rlang::arg_match(
    probes, c("verbosity", "self_preference", "bandwagon"), multiple = TRUE
  )
  p_adjust <- rlang::arg_match(p_adjust)
  flip_threshold <- check_probability(flip_threshold, "flip_threshold")
  alpha <- check_probability(alpha, "alpha", open = TRUE)
  conf_level <- check_probability(conf_level, "conf_level", open = TRUE)
  ordered_levels <- as_labels(ordered_levels)
  if (length(ordered_levels) < 2L || anyNA(ordered_levels) || anyDuplicated(ordered_levels)) {
    yh_abort("{.arg ordered_levels} must contain at least two unique labels.")
  }
  check_labels_nonblank(ordered_levels, "ordered_levels")

  data <- tibble::as_tibble(data)
  check_probe_input(data)
  if (nrow(data) < 1L) yh_abort("{.arg data} must contain at least one item.")
  # Identifiers are validated on the full data: a subsample could hide a
  # duplicate that the caller's audit trail would later trip over.
  validate_source_ids(data, id_col, "data")
  seed <- check_seed(seed)
  if (!is.null(seed)) withr::local_seed(seed)
  if (!is.null(n)) {
    n <- check_count(n, "n")
    if (n >= nrow(data)) {
      if (n > nrow(data)) {
        cli::cli_warn("Requested n = {n}; using all {nrow(data)} available rows.")
      }
    } else {
      data <- data[sort(sample.int(nrow(data), n)), , drop = FALSE]
    }
  }
  judge_base <- redacted_judge_input(data, input_cols, id_col = id_col)
  if (!"input" %in% names(judge_base)) {
    yh_abort("{.arg input_cols} must include {.field input} for pointwise probes.")
  }

  rows <- lapply(probes, function(probe) {
    pair <- probe_pair(data$input, probe)
    control <- set_input(judge_base, pair$control)
    treatment <- set_input(judge_base, pair$treatment)
    stacked <- vctrs::vec_rbind(control, treatment)
    scored <- grade_blind(
      judge, stacked, ordered_levels, levels_arg = "ordered_levels"
    )$label
    before <- to_ordinal(scored[seq_len(nrow(data))], ordered_levels)
    after <- to_ordinal(scored[nrow(data) + seq_len(nrow(data))], ordered_levels)
    test <- paired_flip_test(before, after, conf_level)
    bias_result_row(
      probe, pointwise_stats(test),
      conf_level = conf_level, alpha = alpha, flip_threshold = flip_threshold
    )
  })
  rows <- finalize_bias_rows(do.call(vctrs::vec_rbind, rows), p_adjust)
  new_yourhonor_bias(rows, audit_id = audit_id)
}

#' Probe a pairwise judge for position bias
#'
#' Judges every pair in original and swapped display order. Display-space choices
#' are mapped back to stable candidate identity before consistency is measured,
#' so a content-consistent judge remains consistent and an always-first judge is
#' exposed.
#'
#' @param data A data frame with separate character candidate columns (a
#'   list, factor, numeric or matrix column is an error). Missing
#'   candidates are an error, as for [probe_bias()]: a pair with an `NA`
#'   candidate would otherwise be graded as a real comparison and hide
#'   abstention from the missingness gate.
#' @param judge A judge function receiving an opaque, unique `.item_id`, neutral
#'   `.candidate_1` and `.candidate_2` display slots, and any `context_cols`.
#'   Source column names, condition, repeat, and canonical-pair identities are
#'   retained internally and never exposed. The judge returns display-space
#'   choices such as `"A"`, `"B"`, or the optional tie label.
#' @param candidate_a_col,candidate_b_col Column names containing canonical
#'   candidates A and B.
#' @param context_cols Optional context columns passed through unchanged.
#' @param choice_levels Two display labels for the first and second candidate.
#' @param tie_label Optional tie label.
#' @param repeats Number of original/swapped replications per item. Each
#'   condition's judgment is the mode across repeats, so with an even number
#'   a split vote ties and leaves the item unscored. An odd number avoids
#'   two-way ties when the judge returns no tie label and no `NA`; an
#'   acceptance policy counts unscored items against the probe in its upper
#'   bounds, so they can keep it from passing, and so do scored items whose
#'   missing repeats could change their outcome, which it reads from the
#'   `evidence` attribute (see [acceptance_policy()]'s
#'   `max_probe_flip_rate`). Taking the mode also
#'   attenuates a bias that appears on a minority of calls: a judge that,
#'   independently on each call, takes the first slot with probability
#'   `q < 0.5` and otherwise picks the better candidate shows a modal flip
#'   rate of `3q^2 - 2q^3` with three repeats, against `q` with one.
#' @param seed Optional R random seed used to randomize condition order.
#' @param conf_level Confidence level for exact binomial intervals.
#' @param alpha Significance level for first-position preference.
#' @param audit_id Optional evidence-bundle identifier. Use the same value as
#'   the related calibration and stability run.
#'
#' @inheritSection probe_bias Result columns
#'
#' @return A one-row `yourhonor_bias` tibble (see *Result columns*) with the
#'   attributes `audit_id`, `evidence`, and `item_evidence`. Both evidence
#'   attributes are lists with one `"position"` element: `evidence` holds one
#'   row per judgment (condition, repeat, display and canonical choice), and
#'   records `tie_label` as its attribute of that name (`NA` when none), so
#'   an acceptance policy knows which answers a missing judgment could have
#'   been;
#'   `item_evidence` holds one row per item (modal choice per condition,
#'   `flipped`, `display_first_rate`, `position_direction`).
#' @export
#' @examples
#' pairs <- tibble::tibble(
#'   question = paste("question", 1:20),
#'   answer_a = paste("A complete, sourced answer to question", 1:20),
#'   answer_b = paste("A vague reply to question", 1:20)
#' )
#' # Picks the complete answer wherever it is shown: no position bias.
#' content_judge <- function(d) {
#'   ifelse(grepl("complete", d$.candidate_1), "A", "B")
#' }
#' fair <- probe_position(pairs, content_judge, "answer_a", "answer_b",
#'                        context_cols = "question", seed = 1)
#' fair[c("flip_rate", "direction", "position_preference", "verdict")]
#'
#' # Always picks the first slot, which swapping the display order exposes.
#' first_judge <- function(d) rep("A", nrow(d))
#' biased <- probe_position(pairs, first_judge, "answer_a", "answer_b",
#'                          seed = 1)
#' biased[c("flip_rate", "direction", "position_preference", "verdict")]
probe_position <- function(data,
                           judge,
                           candidate_a_col,
                           candidate_b_col,
                           context_cols = NULL,
                           choice_levels = c("A", "B"),
                           tie_label = NULL,
                           repeats = 2,
                           seed = NULL,
                           conf_level = 0.95,
                           alpha = 0.05,
                           audit_id = NULL) {
  if (!is.function(judge)) yh_abort("{.arg judge} must be a function.")
  audit_id <- check_audit_id(audit_id)
  candidate_a_col <- check_column_name(candidate_a_col, "candidate_a_col")
  candidate_b_col <- check_column_name(candidate_b_col, "candidate_b_col")
  if (identical(candidate_a_col, candidate_b_col)) {
    yh_abort("Candidate columns must be two distinct column names.")
  }
  if (!is.null(context_cols)) context_cols <- check_column_names(context_cols, "context_cols")
  visible_cols <- c(candidate_a_col, candidate_b_col, context_cols)
  if (anyDuplicated(visible_cols)) {
    yh_abort("Candidate and context columns must be distinct.")
  }
  # Every name the internal evidence table assigns is reserved, not only the
  # judge-facing ones, so a caller's column is never overwritten in `ev`.
  reserved <- intersect(
    visible_cols,
    c(
      ".item_id", ".condition", ".repeat", ".display_id", ".judge_item_id",
      ".candidate_1", ".candidate_2", "display_choice", "canonical_choice"
    )
  )
  if (length(reserved) > 0L) {
    yh_abort("Reserved internal column{?s} cannot be judge inputs: {.field {reserved}}.")
  }
  repeats <- check_count(repeats, "repeats")
  conf_level <- check_probability(conf_level, "conf_level", open = TRUE)
  alpha <- check_probability(alpha, "alpha", open = TRUE)
  choice_levels <- as_labels(choice_levels)
  if (length(choice_levels) != 2L || anyNA(choice_levels) || anyDuplicated(choice_levels)) {
    yh_abort("{.arg choice_levels} must contain two unique display labels.")
  }
  check_labels_nonblank(choice_levels, "choice_levels")
  if (!is.null(tie_label)) {
    if (length(tie_label) != 1L || is.na(tie_label) || !nzchar(as.character(tie_label))) {
      yh_abort("{.arg tie_label} must be one non-empty label or {.code NULL}.")
    }
    tie_label <- as.character(tie_label)
    check_labels_nonblank(tie_label, "tie_label")
    if (tie_label %in% choice_levels) {
      yh_abort("{.arg tie_label} must differ from {.arg choice_levels}.")
    }
  }
  data <- tibble::as_tibble(data)
  required <- c(candidate_a_col, candidate_b_col, context_cols)
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0L) {
    yh_abort("Missing pairwise column{?s}: {.field {missing_cols}}.")
  }
  if (nrow(data) < 1L) yh_abort("{.arg data} must contain at least one pair.")
  check_candidates_present(data, c(candidate_a_col, candidate_b_col))
  seed <- check_seed(seed)
  if (!is.null(seed)) withr::local_seed(seed)

  evidence <- build_position_evidence(
    data, candidate_a_col, candidate_b_col, context_cols, repeats
  )
  graded <- grade_blind(
    judge, evidence$judge_data, c(choice_levels, tie_label),
    context = "Structured position output", levels_arg = "choice_levels"
  )
  ev <- evidence$internal
  ev$.judge_item_id <- graded$blinding_map$.item_id[
    match(seq_len(nrow(ev)), graded$blinding_map$.source_row)
  ]
  ev$display_choice <- graded$label
  ev$canonical_choice <- map_canonical_choice(
    ev$display_choice, ev$.condition, choice_levels, tie_label
  )

  item_evidence <- position_item_evidence(ev, choice_levels)
  row <- bias_result_row(
    "position", position_stats(item_evidence, ev, conf_level),
    conf_level = conf_level, alpha = alpha, flip_threshold = NA_real_
  )
  # A single probe has no family to correct over; combine_bias() re-adjusts.
  rows <- finalize_bias_rows(row, "none")
  ev <- tibble::as_tibble(ev)
  # The answers a missing judgment could have been: an acceptance policy
  # completes missing repeats with the tie label only when one was in use.
  attr(ev, "tie_label") <- if (is.null(tie_label)) NA_character_ else tie_label
  new_yourhonor_bias(
    rows,
    audit_id = audit_id,
    evidence = list(position = ev),
    item_evidence = list(position = item_evidence)
  )
}

#' @export
print.yourhonor_bias <- function(x, ...) {
  cli::cli_h2("yourhonor bias probes")
  NextMethod()
}

#' Combine bias-probe evidence
#'
#' Combines pointwise and position results while preserving and validating their
#' shared `audit_id`. Use this when an acceptance policy requires probes from
#' both [probe_bias()] and [probe_position()].
#'
#' @param ... Two or more `yourhonor_bias` objects.
#' @param p_adjust Multiple-comparison correction applied across all combined
#'   probes. This replaces the within-call adjusted p-values, and verdicts are
#'   recomputed from the new `p_value_adj` with each row's own `alpha` and
#'   `flip_threshold`.
#'
#' @inheritSection probe_bias Result columns
#'
#' @return One `yourhonor_bias` object with one row per unique probe. The
#'   `evidence` and `item_evidence` attributes are the constituents' lists,
#'   concatenated and still named by probe. Only entries for probes present
#'   in a constituent's rows are carried over, so evidence left behind by
#'   row-subsetting a result cannot shadow a newer run of the same probe.
#' @export
#' @examples
#' length_judge <- function(d) ifelse(nchar(d$input) > 40, "C", "P")
#' pointwise <- probe_bias(
#'   tibble::tibble(input = paste("answer", 1:20)), length_judge,
#'   probes = c("verbosity", "self_preference"), audit_id = "run-1", seed = 1
#' )
#'
#' pairs <- tibble::tibble(
#'   answer_a = paste("A complete answer", 1:20),
#'   answer_b = paste("A vague reply", 1:20)
#' )
#' first_judge <- function(d) rep("A", nrow(d))
#' position <- probe_position(pairs, first_judge, "answer_a", "answer_b",
#'                            audit_id = "run-1", seed = 1)
#'
#' # One family of three probes, Holm-adjusted together.
#' combined <- combine_bias(pointwise, position)
#' combined[c("probe", "direction", "p_value_adj", "verdict")]
combine_bias <- function(...,
                         p_adjust = c("holm", "none", "BH", "bonferroni")) {
  p_adjust <- rlang::arg_match(p_adjust)
  probes <- list(...)
  if (length(probes) < 2L || !all(vapply(probes, inherits, logical(1), "yourhonor_bias"))) {
    yh_abort("{.fn combine_bias} requires two or more {.cls yourhonor_bias} objects.")
  }
  ids <- lapply(probes, audit_id)
  has_id <- !vapply(ids, is.null, logical(1))
  if (any(has_id) && (!all(has_id) || length(unique(unlist(ids))) != 1L)) {
    yh_abort(
      "All bias evidence must share the same non-missing {.arg audit_id}.",
      class = binding_error
    )
  }
  rows <- do.call(vctrs::vec_rbind, lapply(probes, bias_rows))
  if (anyDuplicated(rows$probe)) {
    yh_abort("Combined bias evidence must contain each probe exactly once.")
  }
  if (length(unique(rows$conf_level)) != 1L) {
    yh_abort("Combined bias evidence must use one shared {.arg conf_level}.")
  }
  if (length(unique(rows$alpha)) != 1L) {
    yh_abort("Combined bias evidence must use one shared {.arg alpha}.")
  }
  rows <- finalize_bias_rows(rows, p_adjust)
  new_yourhonor_bias(
    rows,
    audit_id = if (all(has_id)) ids[[1]] else NULL,
    evidence = collect_bias_attr(probes, "evidence"),
    item_evidence = collect_bias_attr(probes, "item_evidence")
  )
}

# Constructor and row schema -------------------------------------------------

new_yourhonor_bias <- function(rows,
                               audit_id = NULL,
                               evidence = list(),
                               item_evidence = list()) {
  rows <- bias_rows(rows)
  out <- tibble::new_tibble(
    rows,
    nrow = nrow(rows),
    class = "yourhonor_bias",
    evidence = probe_named_list(evidence, rows$probe),
    item_evidence = probe_named_list(item_evidence, rows$probe)
  )
  # NULL leaves the attribute absent, which audit_id() returns as NULL and
  # evidence_is_bound() treats as "unbound".
  attr(out, "audit_id") <- audit_id
  out
}

# Strip the class and attributes so rows can be bound and rebuilt.
bias_rows <- function(x) {
  columns <- lapply(names(x), function(col) x[[col]])
  columns <- rlang::set_names(columns, names(x))
  tibble::new_tibble(columns, nrow = nrow(x))
}

probe_named_list <- function(x, probes) {
  x <- x[!vapply(x, is.null, logical(1))]
  if (length(x) == 0L) return(rlang::set_names(list(), character(0)))
  if (is.null(names(x)) || !all(names(x) %in% probes) ||
      anyDuplicated(names(x))) {
    cli::cli_abort(
      "Internal error: bias evidence must be a list named uniquely by probe."
    )
  }
  x
}

# Row-subsetting a result keeps its attributes, so a constituent can carry
# evidence for probes it no longer has rows for; only entries matching its
# own rows are kept. Names are checked before that filter, otherwise an
# unnamed list would be emptied silently instead of refused. `unname()` stops
# names given to `...` from becoming prefixes such as `position.position`.
collect_bias_attr <- function(probes, which, call = rlang::caller_env()) {
  kept <- lapply(unname(probes), function(x) {
    ev <- attr(x, which, exact = TRUE)
    if (length(ev) > 0L && is.null(names(ev))) {
      yh_abort(
        "Bias {which} attribute must be a list named by probe.", call = call
      )
    }
    ev[names(ev) %in% x$probe]
  })
  do.call(c, kept)
}

# The shared bias row schema: every column a bias result carries, in order,
# with its type. bias_result_row() builds rows from it, and report() and
# write_audit() validate supplied bias evidence against its names, so the
# schema and the validation cannot drift apart.
bias_row_prototype <- list(
  probe = NA_character_,
  flip_rate = NA_real_,
  conf_low = NA_real_,
  conf_high = NA_real_,
  direction = NA_character_,
  directional_rate = NA_real_,
  directional_conf_low = NA_real_,
  directional_conf_high = NA_real_,
  position_preference = NA_real_,
  display_first_rate = NA_real_,
  position_conf_low = NA_real_,
  position_conf_high = NA_real_,
  statistic = NA_integer_,
  n_discordant = NA_integer_,
  n_scored = NA_integer_,
  n_directional_items = NA_integer_,
  n_input = NA_integer_,
  n_unscored = NA_integer_,
  unscored_rate = NA_real_,
  n_unscored_tied = NA_integer_,
  n_items_missing = NA_integer_,
  n_judgments = NA_integer_,
  n_judgments_missing = NA_integer_,
  missing_rate = NA_real_,
  n_comparisons = NA_integer_,
  p_value = NA_real_,
  p_value_adj = NA_real_,
  verdict = NA_character_,
  status = NA_character_,
  conf_level = NA_real_,
  alpha = NA_real_,
  flip_threshold = NA_real_,
  p_adjust_method = NA_character_
)

# One row of the shared bias schema. `stats` supplies the probe-specific
# measurements; anything it omits stays NA, so pointwise and position rows
# always have identical columns.
bias_result_row <- function(probe, stats, conf_level, alpha, flip_threshold) {
  row <- tibble::as_tibble(bias_row_prototype)
  row$probe <- probe
  row$conf_level <- conf_level
  row$alpha <- alpha
  row$flip_threshold <- flip_threshold
  unknown <- setdiff(names(stats), names(row))
  if (length(unknown) > 0L) {
    cli::cli_abort(
      "Internal error: unknown bias column{?s} {.field {unknown}}."
    )
  }
  for (col in names(stats)) {
    row[[col]] <- vctrs::vec_cast(stats[[col]], row[[col]])
  }
  row$unscored_rate <- row$n_unscored / row$n_input
  row$missing_rate <- row$n_judgments_missing / row$n_judgments
  row$status <- if (row$n_scored > 0L) "ok" else "insufficient_data"
  # NA marks "no test possible"; every scored probe carries a real p-value.
  if (row$status != "ok") row$p_value <- NA_real_
  row
}

pointwise_stats <- function(test) {
  c(
    as.list(test),
    list(
      n_directional_items = test$n_discordant,
      # Every unscored pointwise item is one with a missing judgment.
      n_items_missing = test$n_unscored,
      n_unscored_tied = 0L,
      n_judgments = 2L * test$n_input,
      n_comparisons = test$n_input
    )
  )
}

position_stats <- function(item_evidence, ev, conf_level) {
  n_input <- nrow(item_evidence)
  scored <- !is.na(item_evidence$flipped)
  n_scored <- sum(scored)
  n_flips <- sum(item_evidence$flipped[scored])
  flip_ci <- exact_binom_interval(n_flips, n_scored, conf_level)

  # position_direction is NA for items that were not decided in both display
  # orders; only scored items can be directional.
  directional <- !is.na(item_evidence$position_direction) &
    item_evidence$position_direction != 0L
  n_directional <- sum(directional)
  first_count <- sum(item_evidence$position_direction[directional] > 0L)
  first_rate <- if (n_directional > 0L) {
    first_count / n_directional
  } else {
    NA_real_
  }
  first_ci <- exact_binom_interval(first_count, n_directional, conf_level)
  # Every flipped item is directional (a tie commits to neither slot, so a
  # flip always moves towards one slot), so the pointwise cap applies here too.
  directional_ci <- directional_magnitude_interval(
    first_count, n_directional - first_count, n_scored, conf_level,
    upper_cap = flip_ci[[2]]
  )
  display_first_rate <- mean(item_evidence$display_first_rate, na.rm = TRUE)
  if (!is.finite(display_first_rate)) display_first_rate <- NA_real_
  missing_judgment <- is.na(ev$display_choice)

  list(
    flip_rate = if (n_scored > 0L) n_flips / n_scored else NA_real_,
    conf_low = flip_ci[[1]],
    conf_high = flip_ci[[2]],
    direction = position_direction_label(first_rate),
    directional_rate = if (n_scored > 0L) {
      abs(2L * first_count - n_directional) / n_scored
    } else {
      NA_real_
    },
    directional_conf_low = directional_ci[[1]],
    directional_conf_high = directional_ci[[2]],
    position_preference = first_rate,
    display_first_rate = display_first_rate,
    position_conf_low = first_ci[[1]],
    position_conf_high = first_ci[[2]],
    statistic = first_count,
    n_discordant = n_flips,
    n_scored = n_scored,
    n_directional_items = n_directional,
    n_input = n_input,
    n_unscored = n_input - n_scored,
    n_items_missing = length(unique(ev$.item_id[missing_judgment])),
    # Unscored through a tie no answer can break: a display order judged in
    # every repeat whose modal choice is tied, whatever the other order's
    # missing judgments would have been.
    n_unscored_tied = sum(
      !scored & item_evidence$.item_id %in% position_tied_items(ev)
    ),
    n_judgments = nrow(ev),
    n_judgments_missing = sum(missing_judgment),
    n_comparisons = nrow(ev) %/% 2L,
    p_value = sign_test_p_value(first_count, n_directional, n_scored)
  )
}

position_tied_items <- function(ev) {
  orders <- split(
    seq_len(nrow(ev)), list(ev$.item_id, ev$.condition), drop = TRUE
  )
  tied <- vapply(orders, function(i) {
    !anyNA(ev$display_choice[i]) && is_tie(ev$canonical_choice[i])
  }, logical(1))
  unique(ev$.item_id[vapply(orders[tied], `[[`, integer(1), 1L)])
}

# The modal choices one display order can end with over every completion of
# its missing judgments (NA for a tied or empty mode, as modal_value()). A
# completion answers each missing judgment with either candidate, or with
# the tie label when `tie_possible`.
reachable_modes <- function(choices, tie_possible) {
  missing <- sum(is.na(choices))
  observed <- choices[!is.na(choices)]
  if (missing == 0L) return(modal_value(observed))
  values <- c("candidate_a", "candidate_b", "tie")
  counts <- vapply(values, function(v) sum(observed == v), integer(1))
  modes <- character(0)
  for (to_a in 0:missing) {
    for (to_b in 0:(missing - to_a)) {
      to_tie <- missing - to_a - to_b
      if (to_tie > 0L && !tie_possible) next
      total <- counts + c(to_a, to_b, to_tie)
      top <- values[total == max(total)]
      modes <- c(modes, if (length(top) == 1L) top else NA_character_)
    }
  }
  unique(modes)
}

# An item's outcome from its two display orders' modal choices, coded as the
# item evidence codes it: NA unscored, 0 unflipped, 1 towards the first slot,
# -1 towards the second.
position_outcome <- function(original_choice, swapped_choice) {
  if (is.na(original_choice) || is.na(swapped_choice)) return(NA_integer_)
  if (original_choice == swapped_choice) return(0L)
  slots <- c(
    canonical_slot(original_choice, "original"),
    canonical_slot(swapped_choice, "swapped")
  )
  as.integer(sign(sum(slots == 1L) - sum(slots == 2L)))
}

# Scored position items whose missing repeats could change their outcome,
# counted by the directions they can reach, once per bound. For the upper
# bound: `free` (both, or a tie, which it counts as a flip either way),
# `first` or `second` only. For the lower bound, where a tie only takes the
# item out of the denominator and counting it unflipped instead is never
# higher (best_case_lower_bounds()), only the reachable flips matter:
# `lower_free` (both), `lower_first`, `lower_second`, or none (it starts
# unflipped and stays so). `observed_first` and `observed_second` count the
# observed flips among them, and `unscorable` those a completion could leave
# unscored, which the probe count leaves out (settled_scored_count()).
# Completing each order with its observed mode keeps the observed outcome,
# so an item that can also be left unscored has two outcomes and is counted.
# Their modal choice is not evidence the bounds can take as observed: a
# judge could otherwise abstain on the repeats that would have flipped.
position_unsettled_counts <- function(evidence, item_evidence, tie_possible) {
  out <- no_unsettled_items
  orders <- split(
    evidence$canonical_choice,
    list(evidence$.item_id, evidence$.condition), drop = TRUE
  )
  for (i in which(!is.na(item_evidence$flipped))) {
    id <- item_evidence$.item_id[[i]]
    original <- orders[[paste(id, "original", sep = ".")]]
    swapped <- orders[[paste(id, "swapped", sep = ".")]]
    if (!anyNA(original) && !anyNA(swapped)) next
    outcomes <- unique(unlist(lapply(
      reachable_modes(original, tie_possible), function(o) {
        vapply(reachable_modes(swapped, tie_possible), function(s) {
          position_outcome(o, s)
        }, integer(1))
      }
    )))
    if (length(outcomes) < 2L) next
    observed <- item_evidence$position_direction[[i]]
    if (observed > 0L) out$observed_first <- out$observed_first + 1L
    if (observed < 0L) out$observed_second <- out$observed_second + 1L
    if (anyNA(outcomes)) out$unscorable <- out$unscorable + 1L
    flips <- c(1L, -1L) %in% outcomes
    upper <- if (anyNA(outcomes) || all(flips)) {
      "free"
    } else if (flips[[1]]) {
      "first"
    } else {
      "second"
    }
    out[[upper]] <- out[[upper]] + 1L
    if (any(flips)) {
      lower <- paste0(
        "lower_",
        if (all(flips)) "free" else if (flips[[1]]) "first" else "second"
      )
      out[[lower]] <- out[[lower]] + 1L
    }
  }
  out
}

# The unsettled items of a position row, read from its probe's per-judgment
# evidence: none when no scored item can have a missing judgment, and NULL
# when one can but the evidence is absent or does not reproduce the row's
# counts. Every unscored item not lost to an unbreakable tie has a missing
# judgment, so at most `n_items_missing - (n_unscored - n_unscored_tied)`
# scored items do; with nothing missing, or at `repeats = 1`, that is 0.
position_row_unsettled <- function(row, bias) {
  if (!identical(row[["probe"]][[1]], "position")) return(no_unsettled_items)
  if (isTRUE(row_count(row, "n_judgments_missing") == 0)) {
    return(no_unsettled_items)
  }
  scored_with_missing <- row_count(row, "n_items_missing") -
    (row_count(row, "n_unscored") - row_count(row, "n_unscored_tied"))
  if (isTRUE(scored_with_missing <= 0)) return(no_unsettled_items)
  all_evidence <- attr(bias, "evidence", exact = TRUE)
  evidence <- if (is.list(all_evidence) && !is.data.frame(all_evidence)) {
    all_evidence[["position"]]
  }
  item_evidence <- matching_position_evidence(row, evidence)
  if (is.null(item_evidence)) return(NULL)
  # An object from before the tie label was recorded may have used one, and
  # a tie in the evidence shows one was in use whatever the label says.
  tie_label <- attr(evidence, "tie_label", exact = TRUE)
  tie_possible <- !(is.character(tie_label) && length(tie_label) == 1L &&
                      is.na(tie_label)) ||
    any(evidence$canonical_choice == "tie", na.rm = TRUE)
  position_unsettled_counts(evidence, item_evidence, tie_possible)
}

# The scored items of a bias row that every completion of its missing
# judgments keeps scored, which is what `min_probe_n` is compared with: the
# observed `n_scored` less the unsettled items a completion could leave
# unscored, or, when `unsettled` is NULL (no matching evidence), less every
# scored item that may have a missing repeat. Unscored items a completion
# would score are not credited. With none to leave out it is the row's own
# `n_scored`, as it stands.
settled_scored_count <- function(row, unsettled) {
  n_scored <- row$n_scored[[1]]
  if (!is.null(unsettled)) {
    if (unsettled$unscorable == 0L) return(n_scored)
    return(n_scored - unsettled$unscorable)
  }
  scored_with_missing <- row_count(row, "n_items_missing") -
    (row_count(row, "n_unscored") - row_count(row, "n_unscored_tied"))
  n_scored <- row_count(row, "n_scored")
  # A hand-edited count of none or fewer has nothing to leave out.
  if (isTRUE(n_scored <= 0)) return(n_scored)
  n_scored - min(n_scored, max(scored_with_missing, 0))
}

row_count <- function(row, field) {
  value <- row[[field]]
  if (is.numeric(value) && length(value) == 1L && is_finite_number(value[[1]])) {
    as.numeric(value[[1]])
  } else {
    NA_real_
  }
}

# Item evidence rebuilt from per-judgment evidence, or NULL unless the
# rebuild reproduces every count of `row` the bounds rest on.
matching_position_evidence <- function(row, evidence) {
  columns <- c(".item_id", ".condition", "display_choice", "canonical_choice")
  if (!is.data.frame(evidence) || nrow(evidence) < 1L ||
      !all(columns %in% names(evidence))) {
    return(NULL)
  }
  # A list, matrix or raw column cannot be split by item; it is not a probe's.
  if (!all(vapply(evidence[columns], function(column) {
    is.atomic(column) && is.null(dim(column)) && !is.raw(column)
  }, logical(1)))) {
    return(NULL)
  }
  choice <- evidence$canonical_choice
  if (!is.character(choice) || !is.character(evidence$.condition) ||
      anyNA(evidence$.item_id) ||
      !all(evidence$.condition %in% c("original", "swapped")) ||
      !all(choice %in% c("candidate_a", "candidate_b", "tie", NA)) ||
      !identical(is.na(choice), is.na(evidence$display_choice))) {
    return(NULL)
  }
  item_evidence <- position_item_evidence(evidence, character(0))
  stats <- position_stats(item_evidence, evidence, 0.95)
  fields <- c(
    "n_input", "n_scored", "n_discordant", "statistic", "n_directional_items",
    "n_unscored_tied", "n_items_missing", "n_judgments", "n_judgments_missing"
  )
  for (field in fields) {
    if (!identical(row_count(row, field), as.numeric(stats[[field]]))) {
      return(NULL)
    }
  }
  item_evidence
}

position_direction_label <- function(first_rate) {
  if (is.na(first_rate) || first_rate == 0.5) {
    "balanced"
  } else if (first_rate > 0.5) {
    "favors_first"
  } else {
    "favors_second"
  }
}

# Apply the family correction and the verdict rule to a set of rows. Shared by
# probe_bias(), probe_position(), and combine_bias() so the three cannot drift.
finalize_bias_rows <- function(rows, p_adjust) {
  rows$p_value_adj <- adjust_p_values(rows$p_value, p_adjust)
  rows$p_adjust_method <- p_adjust
  rows$verdict <- bias_verdict(
    rows$status, rows$p_value_adj, rows$alpha,
    rows$directional_rate, rows$flip_threshold
  )
  rows
}

adjust_p_values <- function(p, method) {
  if (method == "none") return(p)
  # p.adjust() sizes the family by the non-NA entries, so zero-movement probes
  # (p = 1) count and only insufficient_data probes drop out.
  stats::p.adjust(p, method = method)
}

bias_verdict <- function(status,
                         p_value_adj,
                         alpha,
                         directional_rate,
                         flip_threshold) {
  significant <- is.finite(p_value_adj) & p_value_adj < alpha
  # An NA threshold means the probe declares no effect-size floor.
  enough_effect <- is.na(flip_threshold) |
    (is.finite(directional_rate) & directional_rate > flip_threshold)
  ifelse(
    status != "ok", "INCONCLUSIVE",
    ifelse(significant & enough_effect, "BIAS", "no_signal")
  )
}

# Pointwise helpers ------------------------------------------------------------

check_probe_input <- function(data) {
  if (!"input" %in% names(data)) {
    yh_abort(
      "{.arg data} must contain an {.field input} column to perturb."
    )
  }
  if (!is.character(data$input)) {
    yh_abort(paste0(
      "{.field input} must be a character column, ",
      "not {.cls {class(data$input)}}."
    ))
  }
  n_na <- sum(is.na(data$input))
  if (n_na > 0L) {
    # paste0() would turn NA into the literal text "NA", which the judge then
    # grades as a real answer with missing_rate 0.
    yh_abort(paste0(
      "{.field input} contains {n_na} missing value{?s}; ",
      "drop or fill them before probing."
    ))
  }
  invisible(data)
}

# A missing candidate reaches the judge as an empty display slot, and the pair
# is then scored like any other with missing_rate 0, so abstention the caller
# meant to record would be hidden from the missingness gate.
check_candidates_present <- function(data, candidate_cols) {
  for (col in candidate_cols) {
    # The judge contract is candidate text in each display slot, as
    # probe_bias()'s is a character `input`. A list column passes the NA
    # check below (is.na(list(NULL)) is FALSE) and would reach the judge as a
    # list; a matrix column is several candidates per row.
    value <- data[[col]]
    if (!is.character(value) || !is.null(dim(value))) {
      yh_abort(paste0(
        "Candidate column {.field {col}} must be a character vector (one ",
        "candidate per row, with no {.code dim}), not {.cls {class(value)}}."
      ))
    }
    n_na <- sum(is.na(value))
    if (n_na > 0L) {
      yh_abort(paste0(
        "Candidate column {.field {col}} contains {n_na} missing value{?s}; ",
        "drop or fill them before probing."
      ))
    }
  }
  invisible(data)
}

set_input <- function(data, input) {
  data$input <- input
  data
}

probe_pair <- function(input, type) {
  switch(
    type,
    verbosity = list(
      control = input,
      treatment = paste0(input, "\n\n", verbosity_filler())
    ),
    self_preference = list(
      control = paste0(self_pref_prefix("peer"), input),
      treatment = paste0(self_pref_prefix("self"), input)
    ),
    bandwagon = list(
      control = paste0(bandwagon_prefix("wrong"), input),
      treatment = paste0(bandwagon_prefix("right"), input)
    ),
    cli::cli_abort("Unknown probe {.val {type}}.")
  )
}

self_pref_prefix <- function(which) {
  paste0("(For context: the response below was written by ", which, ".)\n\n")
}

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

to_ordinal <- function(labels, ordered_levels) {
  match(as.character(labels), ordered_levels)
}

paired_flip_test <- function(before, after, conf_level = 0.95) {
  n_input <- length(before)
  n_judgments_missing <- sum(is.na(before)) + sum(is.na(after))
  keep <- !is.na(before) & !is.na(after)
  d <- after[keep] - before[keep]
  improved <- sum(d > 0)
  worsened <- sum(d < 0)
  discordant <- improved + worsened
  n_scored <- length(d)
  # With nothing scored the rates are undefined (0/0), not zero.
  flip_rate <- if (n_scored > 0L) discordant / n_scored else NA_real_
  directional_rate <- if (n_scored > 0L) {
    abs(improved - worsened) / n_scored
  } else {
    NA_real_
  }
  ci <- exact_binom_interval(discordant, n_scored, conf_level)
  directional_ci <- directional_magnitude_interval(
    improved, worsened, n_scored, conf_level, upper_cap = ci[[2]]
  )
  tibble::tibble(
    flip_rate = flip_rate,
    conf_low = ci[[1]],
    conf_high = ci[[2]],
    direction = flip_direction(improved, worsened),
    directional_rate = directional_rate,
    directional_conf_low = directional_ci[[1]],
    directional_conf_high = directional_ci[[2]],
    statistic = improved,
    n_discordant = discordant,
    n_scored = n_scored,
    n_input = n_input,
    n_unscored = n_input - n_scored,
    n_judgments_missing = n_judgments_missing,
    p_value = sign_test_p_value(improved, discordant, n_scored)
  )
}

flip_direction <- function(improved, worsened) {
  if (improved + worsened == 0L) {
    "none"
  } else if (improved > worsened) {
    "favors_perturbed"
  } else if (worsened > improved) {
    "favors_original"
  } else {
    "balanced"
  }
}

sign_test_p_value <- function(successes, trials, n_scored) {
  if (n_scored == 0L) return(NA_real_)
  # No movement at all is the strongest null outcome the probe can produce, so
  # it stays in the multiple-comparison family as p = 1 instead of vanishing
  # as NA and shrinking the family for the other probes.
  if (trials == 0L) return(1)
  stats::binom.test(successes, trials, 0.5)$p.value
}

directional_magnitude_interval <- function(positive,
                                           negative,
                                           n,
                                           conf_level,
                                           upper_cap = 1) {
  if (n < 1L) return(c(NA_real_, NA_real_))
  signed <- signed_difference_interval(positive, negative, n, conf_level)
  # |p+ - p-| <= p+ + p-, so a bound on the any-direction rate over the same
  # denominator is also a valid, usually tighter, bound on the net rate.
  c(magnitude_lower(signed), min(1, max(abs(signed)), upper_cap))
}

# Bonferroni-combine exact marginal intervals for the two multinomial cells.
# The resulting interval for their signed difference is conservative. Both
# endpoints rise with `positive` and fall with `negative` at a fixed `n`.
signed_difference_interval <- function(positive, negative, n, conf_level) {
  marginal_level <- 1 - (1 - conf_level) / 2
  positive_ci <- exact_binom_interval(positive, n, marginal_level)
  negative_ci <- exact_binom_interval(negative, n, marginal_level)
  c(
    positive_ci[[1]] - negative_ci[[2]],
    positive_ci[[2]] - negative_ci[[1]]
  )
}

# The lower bound on |p+ - p-| a signed interval gives: 0 when it contains 0.
magnitude_lower <- function(signed) {
  if (signed[[1]] <= 0 && signed[[2]] >= 0) 0 else min(abs(signed))
}

# A bias row's counts, each a whole non-negative number that adds up with
# the others, or NULL. Every count is checked to be one finite number before
# any arithmetic, so a missing, character or multi-valued field gives NULL
# (an unusable bound) rather than an error or a dropped value.
bias_row_counts <- function(row) {
  fields <- c(
    n_input = "n_input", n_scored = "n_scored", n_unscored = "n_unscored",
    n_discordant = "n_discordant", positive = "statistic",
    n_directional = "n_directional_items"
  )
  values <- lapply(fields, function(field) {
    value <- row[[field]]
    if (is.numeric(value) && length(value) == 1L) value[[1]] else NA_real_
  })
  if (!all(vapply(values, is_finite_number, logical(1)))) return(NULL)
  counts <- c(values, list(negative = values$n_directional - values$positive))
  counts$n_directional <- NULL
  numbers <- unlist(counts)
  if (any(numbers != floor(numbers)) || any(numbers < 0) ||
      counts$n_scored < 1L ||
      counts$n_unscored != counts$n_input - counts$n_scored ||
      counts$n_discordant > counts$n_scored ||
      counts$positive + counts$negative > counts$n_discordant) {
    return(NULL)
  }
  counts
}

# Upper bounds on a bias row's flip and net directional rates over every
# supplied item, counting each unscored item as a flip in whichever direction
# is worse. An unscored item (a missing judgment, or a position item lost to a
# within-condition tie) is one the probe could not resolve, and a judge's
# indecision can carry the very bias the probe measures, so the policy cannot
# treat it as agreement. With nothing unscored these equal the row's own
# `conf_high` and `directional_conf_high`. Counts that are missing or do not
# add up give `NA` bounds, which the policy reads as an unusable interval.
#
# `unsettled` (position_unsettled_counts()) names the scored position items
# whose missing repeats could change their outcome: their observed flips are
# taken out of the counts, and each is then a flip in whichever direction its
# repeats can reach (`free` items either way, `first` and `second` items only
# that way). With none, the bounds are exactly the row's counts' ones.
worst_case_upper_bounds <- function(row, unsettled = no_unsettled_items) {
  counts <- bias_row_counts(row)
  conf_level <- row$conf_level[[1]]
  counts <- settled_counts(counts, unsettled)
  if (is.null(counts) || !is_finite_number(conf_level)) {
    return(list(flip = NA_real_, directional = NA_real_))
  }
  n_input <- counts$n_input
  n_unscored <- counts$n_unscored
  positive <- counts$positive
  negative <- counts$negative
  flip_high <- exact_binom_interval(
    counts$n_discordant + unsettled_total(unsettled) + n_unscored, n_input,
    conf_level
  )[[2]]
  # Moving an item from the negative cell to the positive one raises both
  # signed endpoints, so the most positive upper endpoint is at the
  # all-positive split and the most negative lower endpoint at the
  # all-negative one: the larger absolute endpoint of those two bounds every
  # split. An unsettled item that cannot reach a direction stays unflipped
  # in that split, which keeps that endpoint highest.
  directional_high <- max(
    directional_magnitude_interval(
      positive + n_unscored + unsettled$free + unsettled$first, negative,
      n_input, conf_level, upper_cap = flip_high
    )[[2]],
    directional_magnitude_interval(
      positive, negative + n_unscored + unsettled$free + unsettled$second,
      n_input, conf_level, upper_cap = flip_high
    )[[2]]
  )
  list(flip = flip_high, directional = directional_high)
}

# No unsettled items: the bounds use the row's counts as they stand.
no_unsettled_items <- list(
  free = 0L, first = 0L, second = 0L,
  lower_free = 0L, lower_first = 0L, lower_second = 0L,
  observed_first = 0L, observed_second = 0L, unscorable = 0L
)

unsettled_total <- function(unsettled) {
  unsettled$free + unsettled$first + unsettled$second
}

# The row's counts with the unsettled items' observed flips taken out, so the
# remaining cells hold only flips no completion can undo. NULL when the row's
# counts are unusable or the unsettled items do not fit inside them.
settled_counts <- function(counts, unsettled) {
  if (is.null(counts)) return(NULL)
  total <- unsettled_total(unsettled)
  observed <- unsettled$observed_first + unsettled$observed_second
  if (unsettled$observed_first > counts$positive ||
      unsettled$observed_second > counts$negative ||
      total > counts$n_scored || observed > total) {
    return(NULL)
  }
  counts$positive <- counts$positive - unsettled$observed_first
  counts$negative <- counts$negative - unsettled$observed_second
  counts$n_discordant <- counts$n_discordant - observed
  counts
}

# Lower bounds on a bias row's flip and net directional rates under the
# completion of its missing judgments most favourable to the judge, so a FAIL
# on either rests on evidence no answers for them could clear. Only unscored
# items with a missing judgment are completed; an item lost to a
# within-condition tie no answer could break (`n_unscored_tied`, even with a
# missing judgment in its other display order) is decided evidence
# and stays out of the denominator as it does for the scored items' own
# `conf_low`, so the bounds are over `n_input - n_unscored_tied` items. The
# flip rate is least when every completed item is unflipped. The net rate is
# least when completed items cancel the observed direction: the minimum over
# every split of them into flips each way and non-flips. Adding to the
# dominant cell moves the signed interval away from 0, so that minimum lies
# on one of the two one-sided families, (positive, negative + k) and
# (positive + k, negative) for k in 0..n_completed; along each the two signed
# endpoints fall (rise) with k, and an exact lower endpoint at count x + 1
# lies below the upper endpoint at x, so the interval cannot step over 0: the
# family reaches 0 when its end interval reaches 0, and otherwise is least at
# k = 0 or k = n_completed. With nothing to complete these are the row's own
# `conf_low` and `directional_conf_low`. Counts that are missing or do not
# add up give `NA` bounds, as in worst_case_upper_bounds().
#
# Unsettled scored position items (`unsettled`, see
# worst_case_upper_bounds()) are completed too: each starts unflipped and
# may flip only in a direction its repeats can reach (`lower_*`). Starting
# unflipped is safe even where no completion leaves the item unflipped:
# an extra outcome can only lower a minimum, and a completion that ties the
# item leaves it out of that completion's denominator, which counting it
# unflipped is never above (for the flip rate, an exact lower endpoint falls
# as n grows at a fixed count; for the net rate a test checks it over every
# pair of counts, n up to 40). The families then start from the
# settled counts, and the one adding to the negative cell reaches
# `n_completed + lower_free + lower_second` while the other reaches
# `n_completed + lower_free + lower_first`. The least over every
# allocation still lies on them: from any
# allocation whose signed interval is above 0, emptying the positive cell
# down to the settled count lowers both endpoints, landing on the negative
# family either still above 0 (no higher) or, because the interval cannot
# step over 0, after passing an interval that contains it, in which case one
# of the two families contains one as well; likewise below 0. So the
# bounds stay at or below every completion's.
best_case_lower_bounds <- function(row, unsettled = no_unsettled_items) {
  unusable <- list(flip = NA_real_, directional = NA_real_)
  counts <- bias_row_counts(row)
  conf_level <- row$conf_level[[1]]
  counts <- settled_counts(counts, unsettled)
  if (is.null(counts) || !is_finite_number(conf_level)) return(unusable)
  n_unsettled <- unsettled_total(unsettled)
  if (counts$n_unscored == 0L && n_unsettled == 0L) {
    return(list(
      flip = row$conf_low[[1]], directional = row$directional_conf_low[[1]]
    ))
  }
  tied <- row[["n_unscored_tied"]]
  if (!is.numeric(tied) || length(tied) != 1L || !is_finite_number(tied[[1]]) ||
      tied != floor(tied) || tied < 0 || tied > counts$n_unscored) {
    return(unusable)
  }
  # Only the position probe has ties; a pointwise item is unscored only
  # through a missing judgment.
  if (tied > 0 && !identical(row[["probe"]][[1]], "position")) {
    return(unusable)
  }
  n_completed <- counts$n_unscored - tied
  if (n_completed == 0L && n_unsettled == 0L) {
    return(list(
      flip = row$conf_low[[1]], directional = row$directional_conf_low[[1]]
    ))
  }
  n <- counts$n_input - tied
  positive <- counts$positive
  negative <- counts$negative
  flip_low <- exact_binom_interval(counts$n_discordant, n, conf_level)[[1]]
  start <- signed_difference_interval(positive, negative, n, conf_level)
  # Completed items as flips against the positive cell, then against the
  # negative one.
  towards_negative <- signed_difference_interval(
    positive, negative + n_completed + unsettled$lower_free +
      unsettled$lower_second, n, conf_level
  )
  towards_positive <- signed_difference_interval(
    positive + n_completed + unsettled$lower_free + unsettled$lower_first,
    negative, n, conf_level
  )
  cancel_positive <- if (start[[2]] < 0) {
    magnitude_lower(start)
  } else if (towards_negative[[1]] <= 0) {
    0
  } else {
    towards_negative[[1]]
  }
  cancel_negative <- if (start[[1]] > 0) {
    magnitude_lower(start)
  } else if (towards_positive[[2]] >= 0) {
    0
  } else {
    -towards_positive[[2]]
  }
  list(flip = flip_low, directional = min(cancel_positive, cancel_negative))
}

# Position helpers -------------------------------------------------------------

build_position_evidence <- function(data,
                                    candidate_a_col,
                                    candidate_b_col,
                                    context_cols,
                                    repeats) {
  keep <- unique(c(context_cols, candidate_a_col, candidate_b_col))
  base <- tibble::as_tibble(data[keep])
  rows <- vector("list", repeats * 2L)
  keys <- vector("list", repeats * 2L)
  index <- 1L
  for (repeat_id in seq_len(repeats)) {
    original <- base
    swapped <- base
    swapped[[candidate_a_col]] <- base[[candidate_b_col]]
    swapped[[candidate_b_col]] <- base[[candidate_a_col]]
    rows[[index]] <- original
    keys[[index]] <- tibble::tibble(
      .item_id = seq_len(nrow(base)), .condition = "original", .repeat = repeat_id
    )
    index <- index + 1L
    rows[[index]] <- swapped
    keys[[index]] <- tibble::tibble(
      .item_id = seq_len(nrow(base)), .condition = "swapped", .repeat = repeat_id
    )
    index <- index + 1L
  }
  key <- do.call(vctrs::vec_rbind, keys)
  displayed <- do.call(vctrs::vec_rbind, rows)
  internal <- tibble::add_column(
    displayed,
    .item_id = key$.item_id,
    .condition = key$.condition,
    .repeat = key$.repeat,
    .display_id = seq_len(nrow(displayed)),
    .before = 1L
  )
  judge_data <- displayed
  names(judge_data)[names(judge_data) == candidate_a_col] <- ".candidate_1"
  names(judge_data)[names(judge_data) == candidate_b_col] <- ".candidate_2"
  list(judge_data = judge_data, internal = internal)
}

map_canonical_choice <- function(choice, condition, choice_levels, tie_label) {
  out <- rep(NA_character_, length(choice))
  original <- condition == "original"
  swapped <- condition == "swapped"
  out[original & choice == choice_levels[[1]]] <- "candidate_a"
  out[original & choice == choice_levels[[2]]] <- "candidate_b"
  out[swapped & choice == choice_levels[[1]]] <- "candidate_b"
  out[swapped & choice == choice_levels[[2]]] <- "candidate_a"
  if (!is.null(tie_label)) out[choice == tie_label] <- "tie"
  out
}

# The display slot a canonical choice occupied in a given condition: 1 for
# the first slot, 2 for the second, 0 for a tie.
canonical_slot <- function(choice, condition) {
  if (choice == "tie") return(0L)
  first <- if (condition == "original") "candidate_a" else "candidate_b"
  if (choice == first) 1L else 2L
}

position_item_evidence <- function(evidence, choice_levels) {
  rows <- lapply(unique(evidence$.item_id), function(item_id) {
    item <- evidence[evidence$.item_id == item_id, , drop = FALSE]
    original <- item$canonical_choice[item$.condition == "original"]
    swapped <- item$canonical_choice[item$.condition == "swapped"]
    original_choice <- modal_value(original)
    swapped_choice <- modal_value(swapped)
    flipped <- if (is.na(original_choice) || is.na(swapped_choice)) {
      NA
    } else {
      original_choice != swapped_choice
    }

    decided <- item$display_choice %in% choice_levels
    first_rate <- if (any(decided)) {
      mean(item$display_choice[decided] == choice_levels[[1]])
    } else {
      NA_real_
    }
    # Direction is evidence only when the item was decided in both display
    # orders and the canonical choice moved with the order. A judge that keeps
    # its canonical choice has no position preference, however unevenly it
    # abstained across repeats, so display counts are not used here. A tie is
    # a decided judgement that commits to neither slot.
    position_direction <- if (is.na(flipped)) {
      NA_integer_
    } else if (!flipped) {
      0L
    } else {
      slots <- c(
        canonical_slot(original_choice, "original"),
        canonical_slot(swapped_choice, "swapped")
      )
      as.integer(sign(sum(slots == 1L) - sum(slots == 2L)))
    }
    tibble::tibble(
      .item_id = item_id,
      original_choice = original_choice,
      swapped_choice = swapped_choice,
      flipped = flipped,
      display_first_rate = first_rate,
      position_direction = position_direction
    )
  })
  do.call(vctrs::vec_rbind, rows)
}
