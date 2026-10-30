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
#' * `n_items_missing`: items with at least one `NA` judgment.
#' * `n_judgments`, `n_judgments_missing`, `missing_rate`: individual judge
#'   outputs, how many were `NA`, and their share. `missing_rate` is gated
#'   directly by [acceptance_policy()]'s `max_probe_missing_rate`; unscored
#'   items also enter the policy's flip-rate and directional upper bounds as
#'   worst-case flips, so the bounds a policy assesses can exceed this row's
#'   `conf_high` and `directional_conf_high`, which describe the scored items.
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
    cli::cli_abort("{.arg judge} must be a function; probes re-grade perturbed inputs.")
  }
  audit_id <- check_audit_id(audit_id)
  if (!is.character(probes) || length(probes) < 1L || anyNA(probes) ||
      anyDuplicated(probes)) {
    cli::cli_abort("{.arg probes} must contain one or more unique probe names.")
  }
  if (any(probes == "position")) {
    cli::cli_abort(paste0(
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
  ordered_levels <- as.character(ordered_levels)
  if (length(ordered_levels) < 2L || anyNA(ordered_levels) || anyDuplicated(ordered_levels)) {
    cli::cli_abort("{.arg ordered_levels} must contain at least two unique labels.")
  }
  check_labels_nonblank(ordered_levels, "ordered_levels")

  data <- tibble::as_tibble(data)
  check_probe_input(data)
  if (nrow(data) < 1L) cli::cli_abort("{.arg data} must contain at least one item.")
  # Identifiers are validated on the full data: a subsample could hide a
  # duplicate that the caller's audit trail would later trip over.
  validate_source_ids(data, id_col, "data")
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
    cli::cli_abort("{.arg input_cols} must include {.field input} for pointwise probes.")
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
#' @param data A data frame with separate candidate columns.
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
#'   acceptance policy counts unscored items against the probe (see
#'   [acceptance_policy()]'s `max_probe_flip_rate`). Taking the mode also
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
#'   row per judgment (condition, repeat, display and canonical choice);
#'   `item_evidence` holds one row per item (modal choice per condition,
#'   `flipped`, `display_first_rate`, `position_direction`).
#' @export
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
  if (!is.function(judge)) cli::cli_abort("{.arg judge} must be a function.")
  audit_id <- check_audit_id(audit_id)
  candidate_a_col <- check_column_name(candidate_a_col, "candidate_a_col")
  candidate_b_col <- check_column_name(candidate_b_col, "candidate_b_col")
  if (identical(candidate_a_col, candidate_b_col)) {
    cli::cli_abort("Candidate columns must be two distinct column names.")
  }
  if (!is.null(context_cols)) context_cols <- check_column_names(context_cols, "context_cols")
  visible_cols <- c(candidate_a_col, candidate_b_col, context_cols)
  if (anyDuplicated(visible_cols)) {
    cli::cli_abort("Candidate and context columns must be distinct.")
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
    cli::cli_abort("Reserved internal column{?s} cannot be judge inputs: {.field {reserved}}.")
  }
  repeats <- check_count(repeats, "repeats")
  conf_level <- check_probability(conf_level, "conf_level", open = TRUE)
  alpha <- check_probability(alpha, "alpha", open = TRUE)
  choice_levels <- as.character(choice_levels)
  if (length(choice_levels) != 2L || anyNA(choice_levels) || anyDuplicated(choice_levels)) {
    cli::cli_abort("{.arg choice_levels} must contain two unique display labels.")
  }
  check_labels_nonblank(choice_levels, "choice_levels")
  if (!is.null(tie_label)) {
    if (length(tie_label) != 1L || is.na(tie_label) || !nzchar(as.character(tie_label))) {
      cli::cli_abort("{.arg tie_label} must be one non-empty label or {.code NULL}.")
    }
    tie_label <- as.character(tie_label)
    check_labels_nonblank(tie_label, "tie_label")
    if (tie_label %in% choice_levels) {
      cli::cli_abort("{.arg tie_label} must differ from {.arg choice_levels}.")
    }
  }
  data <- tibble::as_tibble(data)
  required <- c(candidate_a_col, candidate_b_col, context_cols)
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0L) {
    cli::cli_abort("Missing pairwise column{?s}: {.field {missing_cols}}.")
  }
  if (nrow(data) < 1L) cli::cli_abort("{.arg data} must contain at least one pair.")
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
  new_yourhonor_bias(
    rows,
    audit_id = audit_id,
    evidence = list(position = tibble::as_tibble(ev)),
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
combine_bias <- function(...,
                         p_adjust = c("holm", "none", "BH", "bonferroni")) {
  p_adjust <- rlang::arg_match(p_adjust)
  probes <- list(...)
  if (length(probes) < 2L || !all(vapply(probes, inherits, logical(1), "yourhonor_bias"))) {
    cli::cli_abort("{.fn combine_bias} requires two or more {.cls yourhonor_bias} objects.")
  }
  ids <- lapply(probes, audit_id)
  has_id <- !vapply(ids, is.null, logical(1))
  if (any(has_id) && (!all(has_id) || length(unique(unlist(ids))) != 1L)) {
    cli::cli_abort("All bias evidence must share the same non-missing {.arg audit_id}.")
  }
  rows <- do.call(vctrs::vec_rbind, lapply(probes, bias_rows))
  if (anyDuplicated(rows$probe)) {
    cli::cli_abort("Combined bias evidence must contain each probe exactly once.")
  }
  if (length(unique(rows$conf_level)) != 1L) {
    cli::cli_abort("Combined bias evidence must use one shared {.arg conf_level}.")
  }
  if (length(unique(rows$alpha)) != 1L) {
    cli::cli_abort("Combined bias evidence must use one shared {.arg alpha}.")
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
      cli::cli_abort(
        "Bias {which} attribute must be a list named by probe.", call = call
      )
    }
    ev[names(ev) %in% x$probe]
  })
  do.call(c, kept)
}

# One row of the shared bias schema. `stats` supplies the probe-specific
# measurements; anything it omits stays NA, so pointwise and position rows
# always have identical columns.
bias_result_row <- function(probe, stats, conf_level, alpha, flip_threshold) {
  row <- tibble::tibble(
    probe = probe,
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
    n_items_missing = NA_integer_,
    n_judgments = NA_integer_,
    n_judgments_missing = NA_integer_,
    missing_rate = NA_real_,
    n_comparisons = NA_integer_,
    p_value = NA_real_,
    p_value_adj = NA_real_,
    verdict = NA_character_,
    status = NA_character_,
    conf_level = conf_level,
    alpha = alpha,
    flip_threshold = flip_threshold,
    p_adjust_method = NA_character_
  )
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
    n_judgments = nrow(ev),
    n_judgments_missing = sum(missing_judgment),
    n_comparisons = nrow(ev) %/% 2L,
    p_value = sign_test_p_value(first_count, n_directional, n_scored)
  )
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
    cli::cli_abort(
      "{.arg data} must contain an {.field input} column to perturb."
    )
  }
  if (!is.character(data$input)) {
    cli::cli_abort(paste0(
      "{.field input} must be a character column, ",
      "not {.cls {class(data$input)}}."
    ))
  }
  n_na <- sum(is.na(data$input))
  if (n_na > 0L) {
    # paste0() would turn NA into the literal text "NA", which the judge then
    # grades as a real answer with missing_rate 0.
    cli::cli_abort(paste0(
      "{.field input} contains {n_na} missing value{?s}; ",
      "drop or fill them before probing."
    ))
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
  # Bonferroni-combine exact marginal intervals for the two multinomial cells.
  # The resulting interval for their signed difference is conservative.
  marginal_level <- 1 - (1 - conf_level) / 2
  positive_ci <- exact_binom_interval(positive, n, marginal_level)
  negative_ci <- exact_binom_interval(negative, n, marginal_level)
  signed <- c(
    positive_ci[[1]] - negative_ci[[2]],
    positive_ci[[2]] - negative_ci[[1]]
  )
  low <- if (signed[[1]] <= 0 && signed[[2]] >= 0) {
    0
  } else {
    min(abs(signed))
  }
  # |p+ - p-| <= p+ + p-, so a bound on the any-direction rate over the same
  # denominator is also a valid, usually tighter, bound on the net rate.
  c(low, min(1, max(abs(signed)), upper_cap))
}

# Upper bounds on a bias row's flip and net directional rates over every
# supplied item, counting each unscored item as a flip in whichever direction
# is worse. An unscored item (a missing judgment, or a position item lost to a
# within-condition tie) is one the probe could not resolve, and a judge's
# indecision can carry the very bias the probe measures, so the policy cannot
# treat it as agreement. With nothing unscored these equal the row's own
# `conf_high` and `directional_conf_high`. Counts that are missing or do not
# add up give `NA` bounds, which the policy reads as an unusable interval.
worst_case_upper_bounds <- function(row) {
  unusable <- list(flip = NA_real_, directional = NA_real_)
  n_input <- row$n_input[[1]]
  n_scored <- row$n_scored[[1]]
  n_unscored <- row$n_unscored[[1]]
  n_discordant <- row$n_discordant[[1]]
  conf_level <- row$conf_level[[1]]
  positive <- row$statistic[[1]]
  negative <- row$n_directional_items[[1]] - positive
  counts <- c(n_input, n_scored, n_unscored, n_discordant, positive, negative)
  if (!all(is.finite(counts)) || any(counts != floor(counts)) ||
      any(counts < 0) || n_scored < 1L ||
      n_unscored != n_input - n_scored || n_discordant > n_scored ||
      positive + negative > n_discordant) {
    return(unusable)
  }
  flip_high <- exact_binom_interval(
    n_discordant + n_unscored, n_input, conf_level
  )[[2]]
  # Moving an item from the negative cell to the positive one raises both
  # signed endpoints, so the most positive upper endpoint is at the
  # all-positive split and the most negative lower endpoint at the
  # all-negative one: the larger absolute endpoint of those two bounds every
  # split.
  directional_high <- max(
    directional_magnitude_interval(
      positive + n_unscored, negative, n_input, conf_level,
      upper_cap = flip_high
    )[[2]],
    directional_magnitude_interval(
      positive, negative + n_unscored, n_input, conf_level,
      upper_cap = flip_high
    )[[2]]
  )
  list(flip = flip_high, directional = directional_high)
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
