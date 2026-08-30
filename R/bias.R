#' Probe a pointwise judge for paired bias
#'
#' Grades length-matched control/treatment pairs in one randomized batch and
#' measures directional label movement. Position bias is intentionally handled
#' by [probe_position()], which uses structured candidates and stable identities.
#'
#' @param data A data frame with an `input` column.
#' @param judge A judge function.
#' @param input_cols Columns visible to `judge`. The default exposes only
#'   `input`; all other columns are redacted.
#' @param id_col Optional unique source identifier. It is validated but is not
#'   exposed to the judge unless explicitly included in `input_cols`.
#' @param probes Any of `"verbosity"`, `"self_preference"`, and `"bandwagon"`.
#' @param ordered_levels Label scale from worst to best.
#' @param n Optional positive number of sampled items.
#' @param seed Optional R random seed.
#' @param flip_threshold Minimum directional flip rate used for a `"BIAS"` flag.
#' @param alpha Significance level.
#' @param p_adjust Multiple-comparison correction.
#' @param conf_level Confidence level for the exact flip-rate interval.
#' @param audit_id Optional evidence-bundle identifier. Use the same value as
#'   the related calibration and stability run.
#'
#' @return A `yourhonor_bias` tibble. `"no_signal"` means the probe did not
#'   detect directional bias; it is not proof that the judge is unbiased.
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

  data <- tibble::as_tibble(data)
  if (!"input" %in% names(data)) {
    cli::cli_abort("{.arg data} must contain an {.field input} column to perturb.")
  }
  if (nrow(data) < 1L) cli::cli_abort("{.arg data} must contain at least one item.")
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

  raw <- lapply(probes, function(probe) {
    pair <- probe_pair(data$input, probe)
    control <- set_input(judge_base, pair$control)
    treatment <- set_input(judge_base, pair$treatment)
    stacked <- vctrs::vec_rbind(control, treatment)
    scored <- grade_blind(judge, stacked, ordered_levels)$label
    before <- to_ordinal(scored[seq_len(nrow(data))], ordered_levels)
    after <- to_ordinal(scored[nrow(data) + seq_len(nrow(data))], ordered_levels)
    c(list(probe = probe), paired_flip_test(before, after, conf_level))
  })

  p_value <- vapply(raw, function(r) r$p_value, numeric(1))
  p_value_adj <- p_value
  scored_p <- is.finite(p_value)
  if (p_adjust != "none" && any(scored_p)) {
    p_value_adj[scored_p] <- stats::p.adjust(p_value[scored_p], method = p_adjust)
  }

  rows <- lapply(seq_along(raw), function(i) {
    r <- raw[[i]]
    padj <- p_value_adj[[i]]
    verdict <- if (r$n_scored == 0L) {
      "INCONCLUSIVE"
    } else if (is.finite(padj) && padj < alpha && r$flip_rate > flip_threshold) {
      "BIAS"
    } else {
      "no_signal"
    }
    tibble::tibble(
      probe = r$probe,
      flip_rate = r$flip_rate,
      conf_low = r$conf_low,
      conf_high = r$conf_high,
      direction = r$direction,
      directional_rate = r$directional_rate,
      directional_conf_low = r$directional_conf_low,
      directional_conf_high = r$directional_conf_high,
      statistic = r$statistic,
      n_discordant = r$n_discordant,
      n_scored = r$n_scored,
      n_input = r$n_input,
      n_missing = r$n_missing_pairs,
      pair_missing_rate = r$n_missing_pairs / r$n_input,
      n_judgments = 2L * r$n_input,
      n_judgments_missing = r$n_judgments_missing,
      missing_rate = r$n_judgments_missing / (2L * r$n_input),
      p_value = r$p_value,
      p_value_adj = padj,
      verdict = verdict,
      status = if (r$n_scored == 0L) "insufficient_data" else "ok",
      conf_level = conf_level,
      alpha = alpha,
      flip_threshold = flip_threshold,
      p_adjust_method = p_adjust
    )
  })

  out <- do.call(vctrs::vec_rbind, rows)
  attr(out, "audit_id") <- audit_id
  structure(out, class = c("yourhonor_bias", class(out)))
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
#' @param repeats Number of original/swapped replications per item.
#' @param seed Optional R random seed used to randomize condition order.
#' @param conf_level Confidence level for exact binomial intervals.
#' @param alpha Significance level for first-position preference.
#' @param audit_id Optional evidence-bundle identifier. Use the same value as
#'   the related calibration and stability run.
#'
#' @return A one-row `yourhonor_bias` tibble with item-level evidence attached as
#'   the `"evidence"` attribute.
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
  reserved <- intersect(
    visible_cols,
    c(
      ".item_id", ".condition", ".repeat", ".display_id",
      ".candidate_1", ".candidate_2"
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
  if (!is.null(tie_label)) {
    if (length(tie_label) != 1L || is.na(tie_label) || !nzchar(as.character(tie_label))) {
      cli::cli_abort("{.arg tie_label} must be one non-empty label or {.code NULL}.")
    }
    tie_label <- as.character(tie_label)
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
    context = "Structured position output"
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
  scored <- !is.na(item_evidence$flipped)
  n_scored <- sum(scored)
  n_flips <- sum(item_evidence$flipped[scored])
  flip_rate <- if (n_scored > 0L) n_flips / n_scored else NA_real_
  flip_ci <- exact_binom_interval(n_flips, n_scored, conf_level)

  directional <- !is.na(item_evidence$position_direction) &
    item_evidence$position_direction != 0L
  n_directional <- sum(directional)
  first_count <- sum(item_evidence$position_direction[directional] > 0L)
  first_rate <- if (n_directional > 0L) first_count / n_directional else NA_real_
  first_ci <- exact_binom_interval(first_count, n_directional, conf_level)
  directional_ci <- directional_magnitude_interval(
    first_count, n_directional - first_count, nrow(data), conf_level
  )
  p <- if (n_directional > 0L) {
    stats::binom.test(first_count, n_directional, 0.5)$p.value
  } else {
    NA_real_
  }
  display_first_rate <- mean(item_evidence$display_first_rate, na.rm = TRUE)
  if (!is.finite(display_first_rate)) display_first_rate <- NA_real_
  direction <- if (is.na(first_rate)) {
    "balanced"
  } else if (first_rate > 0.5) {
    "favors_first"
  } else if (first_rate < 0.5) {
    "favors_second"
  } else {
    "balanced"
  }
  verdict <- if (n_scored == 0L) {
    "INCONCLUSIVE"
  } else if (is.finite(p) && p < alpha) {
    "BIAS"
  } else {
    "no_signal"
  }

  out <- tibble::tibble(
    probe = "position",
    flip_rate = flip_rate,
    conf_low = flip_ci[[1]],
    conf_high = flip_ci[[2]],
    direction = direction,
    directional_rate = abs(2L * first_count - n_directional) / nrow(data),
    directional_conf_low = directional_ci[[1]],
    directional_conf_high = directional_ci[[2]],
    position_preference = first_rate,
    display_first_rate = display_first_rate,
    position_conf_low = first_ci[[1]],
    position_conf_high = first_ci[[2]],
    statistic = first_count,
    n_discordant = n_flips,
    n_scored = n_scored,
    n_input = nrow(data),
    n_missing = nrow(data) - n_scored,
    pair_missing_rate = (nrow(data) - n_scored) / nrow(data),
    n_judgments = nrow(ev),
    n_judgments_missing = sum(is.na(ev$display_choice)),
    missing_rate = mean(is.na(ev$display_choice)),
    n_comparisons = nrow(ev) / 2L,
    n_directional_items = n_directional,
    p_value = p,
    p_value_adj = p,
    verdict = verdict,
    status = if (verdict == "INCONCLUSIVE") "insufficient_data" else "ok",
    conf_level = conf_level,
    alpha = alpha,
    flip_threshold = NA_real_,
    p_adjust_method = "none"
  )
  attr(out, "evidence") <- tibble::as_tibble(ev)
  attr(out, "item_evidence") <- item_evidence
  attr(out, "audit_id") <- audit_id
  structure(out, class = c("yourhonor_bias", class(out)))
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
#'   probes. This replaces the within-call adjusted p-values.
#' @return One `yourhonor_bias` object with one row per unique probe. Item-level
#'   evidence is retained in list attributes.
#' @export
combine_bias <- function(...,
                         p_adjust = c("holm", "none", "BH", "bonferroni")) {
  p_adjust <- rlang::arg_match(p_adjust)
  probes <- list(...)
  if (length(probes) < 2L || !all(vapply(probes, inherits, logical(1), "yourhonor_bias"))) {
    cli::cli_abort("{.fn combine_bias} requires two or more {.cls yourhonor_bias} objects.")
  }
  ids <- lapply(probes, function(x) attr(x, "audit_id", exact = TRUE))
  has_id <- !vapply(ids, is.null, logical(1))
  if (any(has_id) && (!all(has_id) || length(unique(unlist(ids))) != 1L)) {
    cli::cli_abort("All bias evidence must share the same non-missing {.arg audit_id}.")
  }
  rows <- lapply(probes, function(x) tibble::as_tibble(as.data.frame(x)))
  out <- do.call(vctrs::vec_rbind, rows)
  if (anyDuplicated(out$probe)) {
    cli::cli_abort("Combined bias evidence must contain each probe exactly once.")
  }
  if (length(unique(out$conf_level)) != 1L) {
    cli::cli_abort("Combined bias evidence must use one shared {.arg conf_level}.")
  }
  if (length(unique(out$alpha)) != 1L) {
    cli::cli_abort("Combined bias evidence must use one shared {.arg alpha}.")
  }
  scored_p <- is.finite(out$p_value)
  out$p_value_adj <- out$p_value
  if (any(scored_p) && p_adjust != "none") {
    out$p_value_adj[scored_p] <- stats::p.adjust(
      out$p_value[scored_p], method = p_adjust
    )
  }
  enough_effect <- out$probe == "position" |
    (is.finite(out$flip_threshold) & out$flip_rate > out$flip_threshold)
  out$verdict <- ifelse(
    out$status != "ok", "INCONCLUSIVE",
    ifelse(
      is.finite(out$p_value_adj) & out$p_value_adj < out$alpha & enough_effect,
      "BIAS", "no_signal"
    )
  )
  out$p_adjust_method <- p_adjust
  attr(out, "audit_id") <- if (all(has_id)) ids[[1]] else NULL
  attr(out, "evidence") <- lapply(probes, attr, which = "evidence", exact = TRUE)
  attr(out, "item_evidence") <- lapply(
    probes, attr, which = "item_evidence", exact = TRUE
  )
  structure(out, class = c("yourhonor_bias", class(out)))
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
  n_missing_pairs <- sum(!keep)
  before <- before[keep]
  after <- after[keep]
  d <- after - before
  improved <- sum(d > 0)
  worsened <- sum(d < 0)
  discordant <- improved + worsened
  n_scored <- length(d)
  flip_rate <- if (n_scored > 0L) discordant / n_scored else NA_real_
  ci <- exact_binom_interval(discordant, n_scored, conf_level)
  directional_ci <- directional_magnitude_interval(
    improved, worsened, n_scored, conf_level
  )

  if (discordant == 0L) {
    return(list(
      flip_rate = flip_rate, conf_low = ci[[1]], conf_high = ci[[2]],
      direction = "none", statistic = 0L, n_discordant = 0L,
      n_scored = n_scored, n_input = n_input,
      n_missing_pairs = n_missing_pairs,
      n_judgments_missing = n_judgments_missing,
      directional_rate = 0,
      directional_conf_low = directional_ci[[1]],
      directional_conf_high = directional_ci[[2]], p_value = NA_real_
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
    flip_rate = flip_rate, conf_low = ci[[1]], conf_high = ci[[2]],
    direction = direction, statistic = improved, n_discordant = discordant,
    n_scored = n_scored, n_input = n_input,
    n_missing_pairs = n_missing_pairs,
    n_judgments_missing = n_judgments_missing,
    directional_rate = abs(improved - worsened) / n_scored,
    directional_conf_low = directional_ci[[1]],
    directional_conf_high = directional_ci[[2]], p_value = p
  )
}

directional_magnitude_interval <- function(positive,
                                           negative,
                                           n,
                                           conf_level) {
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
  c(low, min(1, max(abs(signed))))
}

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
  list(judge_data = judge_data, internal = internal, key = key)
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
    position_direction <- if (is.na(first_rate) || first_rate == 0.5) {
      0L
    } else if (first_rate > 0.5) {
      1L
    } else {
      -1L
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

grade_position <- function(judge, data, choice_levels) {
  grade_blind(
    judge, data, label_levels = choice_levels,
    context = "Structured position output"
  )$label
}
