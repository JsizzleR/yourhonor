test_that("a length-sensitive judge shows verbosity bias with an interval", {
  length_judge <- function(d) ifelse(nchar(d$input) > 40, "C", "P")
  data <- tibble::tibble(input = paste("answer", 1:30))
  res <- probe_bias(data, length_judge, probes = "verbosity", seed = 1)
  expect_s3_class(res, "yourhonor_bias")
  expect_equal(res$probe, "verbosity")
  expect_equal(res$flip_rate, 1)
  expect_equal(res$direction, "favors_perturbed")
  expect_equal(res$directional_rate, 1)
  expect_gt(res$directional_conf_low, 0.7)
  expect_equal(res$verdict, "BIAS")
  expect_lt(res$conf_low, res$flip_rate)
  expect_equal(res$n_input, 30)
  expect_equal(res$missing_rate, 0)
})

test_that("length-matched probes do not convict a length-only judge", {
  length_judge <- function(d) ifelse(nchar(d$input) > 40, "C", "P")
  data <- tibble::tibble(input = paste("answer", 1:30))
  res <- probe_bias(
    data, length_judge,
    probes = c("verbosity", "self_preference", "bandwagon"), seed = 1
  )
  expect_equal(res$verdict[res$probe == "verbosity"], "BIAS")
  expect_true(all(res$verdict[res$probe != "verbosity"] == "no_signal"))
})

test_that("an invariant judge reports no signal, not proof of no bias", {
  flat_judge <- function(d) rep("C", nrow(d))
  data <- tibble::tibble(input = paste("answer", 1:20))
  res <- probe_bias(data, flat_judge, probes = c("verbosity", "self_preference"))
  expect_true(all(res$flip_rate == 0))
  expect_true(all(res$directional_rate == 0))
  expect_true(all(res$directional_conf_high > 0))
  expect_true(all(res$verdict == "no_signal"))
  expect_true(all(res$status == "ok"))
  # No movement is a real sign-test outcome, not a missing one.
  expect_true(all(res$p_value == 1))
})

test_that("probe inputs and controls validate clearly", {
  data <- tibble::tibble(input = "x")
  expect_error(probe_bias(data, NULL), "must be a function")
  expect_error(
    probe_bias(tibble::tibble(text = "x"), function(d) "C"),
    "input"
  )
  expect_error(probe_bias(data, function(d) "C", n = 0), "n")
  expect_error(
    probe_bias(data, function(d) rep("good", nrow(d)),
               ordered_levels = c("I", "P", "C")),
    "outside"
  )
  expect_error(
    probe_bias(data, function(d) "C", probes = "position"),
    "probe_position"
  )
})

test_that("missing or non-character inputs are rejected before grading", {
  calls <- 0L
  judge <- function(d) {
    calls <<- calls + 1L
    rep("C", nrow(d))
  }
  expect_error(
    probe_bias(tibble::tibble(input = c("a", NA, "c")), judge),
    "1 missing value"
  )
  expect_error(
    probe_bias(tibble::tibble(input = 1:3), judge),
    "character column"
  )
  expect_error(
    probe_bias(tibble::tibble(input = factor(c("a", "b"))), judge),
    "character column"
  )
  expect_equal(calls, 0L)
})

test_that("paired flip test handles no discordant pairs", {
  res <- yourhonor:::paired_flip_test(c(1, 2, 3), c(1, 2, 3))
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 1L)
  expect_equal(res$flip_rate, 0)
  expect_equal(res$direction, "none")
  expect_equal(res$n_scored, 3)
  expect_equal(res$directional_rate, 0)
  # Zero discordant pairs with scored items is p = 1, so the probe still
  # counts toward the multiple-comparison family.
  expect_equal(res$p_value, 1)
})

test_that("paired flip test guards the 0/0 case", {
  res <- yourhonor:::paired_flip_test(c(NA, NA), c(1, NA))
  expect_equal(res$n_scored, 0)
  expect_equal(res$n_unscored, 2)
  expect_equal(res$n_judgments_missing, 3)
  expect_true(is.na(res$flip_rate))
  expect_true(is.na(res$directional_rate))
  expect_true(is.na(res$p_value))
})

test_that("bandwagon probe catches a crowd-following judge", {
  crowd_judge <- function(d) ifelse(grepl("looks right", d$input), "C", "P")
  data <- tibble::tibble(input = paste("answer", 1:30))
  res <- probe_bias(data, crowd_judge, probes = "bandwagon", seed = 1)
  expect_equal(res$direction, "favors_perturbed")
  expect_equal(res$verdict, "BIAS")
})

# Flips on every verbosity pair, never on self-preference, and on exactly six
# bandwagon pairs, so the three probes carry distinct p-values.
family_judge <- function(d) {
  small <- grepl("answer 0[1-6]", d$input)
  ifelse(
    grepl("looks right", d$input) & small, "C",
    ifelse(
      grepl("looks wrong", d$input) & small, "I",
      ifelse(nchar(d$input) > 200, "C", "P")
    )
  )
}

family_data <- function() {
  tibble::tibble(input = paste("answer", sprintf("%02d", 1:30)))
}

test_that("p-values are corrected across the whole pointwise family", {
  res <- probe_bias(
    family_data(), family_judge,
    probes = c("verbosity", "self_preference", "bandwagon"), seed = 1
  )
  self <- res[res$probe == "self_preference", ]
  bandwagon <- res[res$probe == "bandwagon", ]
  expect_equal(self$n_discordant, 0)
  expect_equal(self$p_value, 1)
  expect_equal(bandwagon$n_discordant, 6)
  expect_equal(bandwagon$p_value, 0.03125)
  expect_equal(res$p_value_adj, stats::p.adjust(res$p_value, "holm"))
  expect_true(all(res$p_adjust_method == "holm"))
  # Holm over the real family of three lifts bandwagon above alpha; dropping
  # the zero-flip probe from the family would wrongly convict it.
  expect_equal(bandwagon$p_value_adj, 0.0625)
  expect_equal(bandwagon$verdict, "no_signal")
  expect_equal(res$verdict[res$probe == "verbosity"], "BIAS")

  none <- probe_bias(
    family_data(), family_judge,
    probes = c("verbosity", "self_preference", "bandwagon"),
    seed = 1, p_adjust = "none"
  )
  expect_equal(none$p_value_adj, none$p_value)
  expect_true(all(none$p_adjust_method == "none"))
  expect_equal(none$verdict[none$probe == "bandwagon"], "BIAS")
})

test_that("adjust_p_values keeps insufficient-data rows out of the family", {
  p <- c(0.01, NA, 1)
  expect_equal(yourhonor:::adjust_p_values(p, "holm"), c(0.02, NA, 1))
  expect_equal(yourhonor:::adjust_p_values(p, "bonferroni"), c(0.02, NA, 1))
  expect_identical(yourhonor:::adjust_p_values(p, "none"), p)
})

test_that("bias_verdict is vectorised and honours a missing effect floor", {
  verdict <- yourhonor:::bias_verdict(
    status = c("ok", "ok", "ok", "insufficient_data", "ok", "ok"),
    p_value_adj = c(0.01, 0.01, 0.2, NA, 0.01, NA),
    alpha = 0.05,
    directional_rate = c(0.5, 0.05, 0.5, NA, 0.05, 0.5),
    flip_threshold = c(0.1, 0.1, 0.1, 0.1, NA, 0.1)
  )
  expect_identical(
    verdict,
    c("BIAS", "no_signal", "no_signal", "INCONCLUSIVE", "BIAS", "no_signal")
  )
})

test_that("flip_threshold gates on the net directional rate", {
  # 90 items improve, 60 worsen, 150 stay put: flip_rate 0.5 but net 0.1.
  skewed_judge <- function(d) {
    id <- as.integer(substr(d$input, 8, 10))
    long <- nchar(d$input) > 200
    ifelse(
      id <= 90, ifelse(long, "C", "P"),
      ifelse(id <= 150, ifelse(long, "P", "C"), "P")
    )
  }
  data <- tibble::tibble(input = paste("answer", sprintf("%03d", 1:300)))
  strict <- probe_bias(
    data, skewed_judge, probes = "verbosity", seed = 1, flip_threshold = 0.3
  )
  expect_equal(strict$flip_rate, 0.5)
  expect_equal(strict$directional_rate, 0.1)
  expect_lt(strict$p_value_adj, 0.05)
  expect_equal(strict$verdict, "no_signal")

  lenient <- probe_bias(
    data, skewed_judge, probes = "verbosity", seed = 1, flip_threshold = 0.05
  )
  expect_equal(lenient$verdict, "BIAS")
})

test_that("directional interval never exceeds the flip-rate interval", {
  flat_judge <- function(d) rep("C", nrow(d))
  data <- tibble::tibble(input = paste("answer", 1:50))
  res <- probe_bias(data, flat_judge, probes = "verbosity", seed = 1)
  # With zero flips the Bonferroni bound is looser than the flip bound, so the
  # cap is what the row reports.
  expect_equal(res$directional_conf_high, res$conf_high)

  mixed <- probe_bias(
    family_data(), family_judge,
    probes = c("verbosity", "self_preference", "bandwagon"), seed = 1
  )
  expect_true(all(mixed$directional_conf_high <= mixed$conf_high))
  expect_true(all(mixed$directional_conf_low <= mixed$directional_conf_high))
})

test_that("pointwise abstention is counted as unscored and missing items", {
  abstaining <- function(d) {
    ifelse(grepl("answer (1|2|3)$", d$input), NA_character_, "C")
  }
  data <- tibble::tibble(input = paste("answer", 1:20))
  res <- probe_bias(data, abstaining, probes = "verbosity", seed = 1)
  expect_equal(res$n_scored, 17)
  expect_equal(res$n_unscored, 3)
  expect_equal(res$unscored_rate, 3 / 20)
  expect_equal(res$n_items_missing, 3)
  expect_equal(res$n_judgments, 40)
  expect_equal(res$n_judgments_missing, 3)
  expect_equal(res$missing_rate, 3 / 40)
  expect_equal(res$n_comparisons, 20)
  expect_equal(res$status, "ok")
})

pair_data <- function() {
  tibble::tibble(
    context = c("question 1", "question 2"),
    candidate_a = c("good\nmultiline answer", "bad answer"),
    candidate_b = c("bad answer", "good\nmultiline answer")
  )
}

test_that("structured position probe preserves text and canonical identity", {
  seen <- NULL
  content_judge <- function(d) {
    seen <<- names(d)
    ifelse(grepl("good", d$.candidate_1), "A", "B")
  }
  res <- probe_position(
    pair_data(), content_judge,
    "candidate_a", "candidate_b", context_cols = "context", repeats = 2, seed = 1
  )
  expect_equal(res$flip_rate, 0)
  expect_true(is.na(res$position_preference))
  expect_equal(res$display_first_rate, 0.5)
  expect_equal(res$verdict, "no_signal")
  expect_equal(res$p_value, 1)
  expect_identical(
    seen, c(".item_id", "context", ".candidate_1", ".candidate_2")
  )
  expect_false(any(c(".condition", ".repeat", ".display_id") %in% seen))
  evidence <- attr(res, "evidence")$position
  original <- evidence[evidence$.condition == "original", ]
  expect_true(any(grepl("good\nmultiline answer", original$candidate_a, fixed = TRUE)))
  expect_true(all(original$canonical_choice %in% c("candidate_a", "candidate_b")))
})

test_that("position probing redacts everything but the display slots", {
  seen <- NULL
  judge <- function(d) {
    seen <<- names(d)
    ifelse(grepl("good", d$.candidate_1), "A", "B")
  }
  data <- pair_data()
  data$winner <- c("candidate_a", "candidate_b")
  data$human_choice <- c("A", "B")
  data$source_id <- c("pair-1", "pair-2")
  res <- probe_position(
    data, judge, "candidate_a", "candidate_b",
    context_cols = "context", repeats = 2, seed = 1
  )
  expect_equal(res$flip_rate, 0)
  expect_identical(
    seen, c(".item_id", "context", ".candidate_1", ".candidate_2")
  )
  expect_false(any(c("winner", "human_choice", "source_id") %in% seen))
})

test_that("an always-first judge is detected after canonical remapping", {
  first_judge <- function(d) rep("A", nrow(d))
  data <- pair_data()[rep(1:2, 10), ]
  res <- probe_position(
    data, first_judge,
    "candidate_a", "candidate_b", context_cols = "context", repeats = 2, seed = 1
  )
  expect_equal(res$flip_rate, 1)
  expect_equal(res$position_preference, 1)
  expect_equal(res$directional_rate, 1)
  expect_equal(res$n_scored, 20)
  expect_equal(res$n_directional_items, 20)
  expect_equal(res$direction, "favors_first")
  expect_equal(res$verdict, "BIAS")
  expect_equal(res$n_comparisons, 40)
})

test_that("position probes expose raw display-judgment missingness", {
  sometimes_missing <- function(d) {
    ifelse(d$.item_id %% 4L == 0L, NA_character_, "A")
  }
  res <- probe_position(
    pair_data()[rep(1:2, 10), ], sometimes_missing,
    "candidate_a", "candidate_b", context_cols = "context",
    repeats = 2, seed = 1
  )
  expect_equal(res$n_judgments, 80)
  expect_equal(res$n_judgments_missing, 20)
  expect_equal(res$missing_rate, 0.25)
  expect_gt(res$n_items_missing, 0)
  expect_lte(res$n_items_missing, 20)
})

test_that("position indecision is unscored, not missing", {
  coin_judge <- function(d) sample(c("A", "B"), nrow(d), replace = TRUE)
  res <- probe_position(
    pair_data()[rep(1:2, 10), ], coin_judge,
    "candidate_a", "candidate_b", context_cols = "context",
    repeats = 2, seed = 1
  )
  # Within-condition ties across repeats leave items unscored while every
  # judgment was still returned.
  expect_gt(res$n_unscored, 0)
  expect_equal(res$n_unscored, res$n_input - res$n_scored)
  expect_equal(res$unscored_rate, res$n_unscored / res$n_input)
  expect_equal(res$n_items_missing, 0)
  expect_equal(res$n_judgments_missing, 0)
  expect_equal(res$missing_rate, 0)
  expect_equal(res$status, "ok")
})

test_that("structured position outputs can reorder rows safely", {
  judge <- function(d) {
    out <- d[".item_id"]
    out$label <- ifelse(grepl("good", d$.candidate_1), "A", "B")
    out[nrow(out):1, ]
  }
  res <- probe_position(
    pair_data(), judge,
    "candidate_a", "candidate_b", context_cols = "context", repeats = 2, seed = 1
  )
  expect_equal(res$flip_rate, 0)
})

test_that("pointwise grading is redacted, opaque, and keyed", {
  seen_names <- NULL
  seen_ids <- NULL
  data <- tibble::tibble(
    source_id = paste0("label-C-", 1:20),
    input = paste("answer", 1:20),
    human_label = rep("C", 20),
    judge_label = rep("I", 20)
  )
  judge <- function(d) {
    seen_names <<- names(d)
    seen_ids <<- d$.item_id
    tibble::tibble(.item_id = rev(d$.item_id), label = rep("C", nrow(d)))
  }
  res <- probe_bias(
    data, judge, input_cols = "input", id_col = "source_id",
    probes = "verbosity", seed = 1
  )
  expect_equal(res$flip_rate, 0)
  expect_identical(seen_names, c(".item_id", "input"))
  expect_setequal(seen_ids, seq_len(40))
  expect_false(any(c("source_id", "human_label", "judge_label") %in% seen_names))
})

test_that("both probes share one schema and named evidence lists", {
  pointwise <- probe_bias(
    tibble::tibble(input = paste("answer", 1:20)),
    function(d) rep("C", nrow(d)), probes = c("verbosity", "bandwagon"),
    seed = 1
  )
  position <- probe_position(
    pair_data(), function(d) rep("A", nrow(d)),
    "candidate_a", "candidate_b", context_cols = "context", seed = 1
  )
  expect_identical(names(pointwise), names(position))
  expect_identical(
    vapply(pointwise, class, character(1)),
    vapply(position, class, character(1))
  )
  expect_true(all(is.na(pointwise$position_preference)))
  expect_false(is.na(pointwise$flip_threshold[1]))
  expect_true(is.na(position$flip_threshold))

  expect_identical(attr(pointwise, "evidence"), rlang::set_names(list()))
  expect_identical(attr(pointwise, "item_evidence"), rlang::set_names(list()))
  expect_null(attr(pointwise, "audit_id", exact = TRUE))
  expect_named(attr(position, "evidence"), "position")
  expect_named(attr(position, "item_evidence"), "position")
  expect_s3_class(attr(position, "evidence")$position, "tbl_df")
  expect_equal(nrow(attr(position, "item_evidence")$position), 2)
})

test_that("pointwise and position evidence combine only within one audit", {
  pointwise <- probe_bias(
    tibble::tibble(input = paste("answer", 1:20)),
    function(d) rep("C", nrow(d)), probes = "verbosity",
    audit_id = "bundle-1", seed = 1
  )
  position <- probe_position(
    pair_data()[rep(1:2, 10), ], function(d) rep("A", nrow(d)),
    "candidate_a", "candidate_b", context_cols = "context",
    audit_id = "bundle-1", seed = 1
  )
  combined <- combine_bias(pointwise, position)
  expect_s3_class(combined, "yourhonor_bias")
  expect_setequal(combined$probe, c("verbosity", "position"))
  expect_identical(attr(combined, "audit_id"), "bundle-1")
  expect_identical(names(combined), names(pointwise))
  expect_named(attr(combined, "evidence"), "position")
  expect_named(attr(combined, "item_evidence"), "position")

  # Re-combining stays flat: evidence lists are keyed by probe, not nested.
  bandwagon <- probe_bias(
    tibble::tibble(input = paste("answer", 1:20)),
    function(d) rep("C", nrow(d)), probes = "bandwagon",
    audit_id = "bundle-1", seed = 1
  )
  again <- combine_bias(combined, bandwagon)
  expect_setequal(again$probe, c("verbosity", "position", "bandwagon"))
  expect_named(attr(again, "evidence"), "position")
  expect_s3_class(attr(again, "evidence")$position, "tbl_df")

  attr(position, "audit_id") <- "bundle-2"
  expect_error(combine_bias(pointwise, position), "same non-missing")
})

test_that("position direction needs a decided judgement in both display orders", {
  data <- pair_data()[rep(1:2, 10), ]
  # Content-consistent judge that abstains whenever its preferred answer is
  # shown second. Abstention alone must not read as a position preference.
  abstainer <- function(d) ifelse(grepl("good", d$.candidate_1), "A", NA_character_)
  res <- probe_position(
    data, abstainer,
    "candidate_a", "candidate_b", context_cols = "context", repeats = 2, seed = 1
  )
  expect_equal(res$n_scored, 0)
  expect_equal(res$n_directional_items, 0)
  expect_true(is.na(res$directional_rate))
  expect_equal(res$verdict, "INCONCLUSIVE")

  # Same judge, but it answers both orders on the first item only.
  partial <- function(d) {
    ifelse(
      grepl("good", d$.candidate_1) | grepl("question 1", d$context),
      ifelse(grepl("good", d$.candidate_1), "A", "B"),
      NA_character_
    )
  }
  res <- probe_position(
    data, partial,
    "candidate_a", "candidate_b", context_cols = "context", repeats = 2, seed = 1
  )
  expect_equal(res$n_scored, 10)
  expect_equal(res$flip_rate, 0)
  expect_equal(res$n_directional_items, 0)
  expect_equal(res$directional_rate, 0)
  expect_equal(res$verdict, "no_signal")
})

test_that("uneven abstention across repeats is not a position preference", {
  data <- pair_data()[rep(1:2, 10), ]
  # Content-consistent judge that abstains on every other display where its
  # preferred answer is shown second. Its canonical choice never moves.
  uneven <- function(d) {
    preferred_first <- grepl("good", d$.candidate_1)
    ifelse(
      preferred_first, "A",
      ifelse(seq_len(nrow(d)) %% 2L == 0L, "B", NA_character_)
    )
  }
  res <- probe_position(
    data, uneven,
    "candidate_a", "candidate_b", context_cols = "context", repeats = 4, seed = 1
  )
  expect_equal(res$flip_rate, 0)
  expect_equal(res$n_directional_items, 0)
  expect_equal(res$directional_rate, 0)
  expect_equal(res$verdict, "no_signal")
  expect_gt(res$missing_rate, 0)
})

test_that("a tie in one display order still counts toward the slot it avoided", {
  data <- pair_data()[rep(1:2, 10), ]
  # Picks slot 1 when the good answer is first, otherwise declares a tie:
  # every decided judgement commits to slot 1, never to slot 2.
  first_or_tie <- function(d) ifelse(grepl("good", d$.candidate_1), "A", "tie")
  res <- probe_position(
    data, first_or_tie,
    "candidate_a", "candidate_b", context_cols = "context",
    tie_label = "tie", repeats = 2, seed = 1
  )
  expect_equal(res$flip_rate, 1)
  expect_equal(res$n_directional_items, 20)
  expect_equal(res$direction, "favors_first")
  expect_equal(res$verdict, "BIAS")

  second_or_tie <- function(d) ifelse(grepl("good", d$.candidate_2), "B", "tie")
  res <- probe_position(
    data, second_or_tie,
    "candidate_a", "candidate_b", context_cols = "context",
    tie_label = "tie", repeats = 2, seed = 1
  )
  expect_equal(res$direction, "favors_second")
  expect_equal(res$verdict, "BIAS")
})

test_that("blank probe labels are rejected", {
  expect_error(
    probe_bias(tibble::tibble(input = "x"), function(d) "C", ordered_levels = c("", "C")),
    "ordered_levels"
  )
  expect_error(
    probe_position(pair_data(), function(d) "A", "candidate_a", "candidate_b", choice_levels = c(" ", "B")),
    "choice_levels"
  )
  expect_error(
    probe_position(pair_data(), function(d) "A", "candidate_a", "candidate_b", tie_label = " "),
    "tie_label"
  )
})

test_that("position directional rate uses scored items as its denominator", {
  first_judge <- function(d) ifelse(grepl("question 2", d$context), NA_character_, "A")
  res <- probe_position(
    pair_data()[rep(1:2, 10), ], first_judge,
    "candidate_a", "candidate_b", context_cols = "context", repeats = 2, seed = 1
  )
  expect_gt(res$n_unscored, 0)
  expect_equal(res$directional_rate, 1)
  expect_equal(res$n_directional_items, res$n_scored)
})

test_that("combine_bias rejects malformed or inconsistent inputs", {
  data <- tibble::tibble(input = paste("answer", 1:20))
  flat <- function(d) rep("C", nrow(d))
  verbosity <- probe_bias(
    data, flat, probes = "verbosity", audit_id = "bundle-1", seed = 1
  )
  bandwagon <- probe_bias(
    data, flat, probes = "bandwagon", audit_id = "bundle-1", seed = 1
  )
  expect_error(combine_bias(verbosity), "two or more")
  expect_error(
    combine_bias(verbosity, tibble::tibble(probe = "x")), "two or more"
  )
  expect_error(combine_bias(verbosity, verbosity), "exactly once")

  other_level <- probe_bias(
    data, flat, probes = "bandwagon", audit_id = "bundle-1", seed = 1,
    conf_level = 0.9
  )
  expect_error(combine_bias(verbosity, other_level), "conf_level")

  other_alpha <- probe_bias(
    data, flat, probes = "bandwagon", audit_id = "bundle-1", seed = 1,
    alpha = 0.1
  )
  expect_error(combine_bias(verbosity, other_alpha), "alpha")

  expect_silent(combine_bias(verbosity, bandwagon))
})

test_that("combine_bias recomputes the family correction and verdicts", {
  pointwise <- probe_bias(
    family_data(), family_judge,
    probes = c("verbosity", "self_preference", "bandwagon"),
    audit_id = "bundle-1", seed = 1
  )
  position <- probe_position(
    pair_data()[rep(1:2, 10), ], function(d) rep("A", nrow(d)),
    "candidate_a", "candidate_b", context_cols = "context",
    audit_id = "bundle-1", seed = 1
  )
  combined <- combine_bias(pointwise, position, p_adjust = "bonferroni")
  expect_equal(
    combined$p_value_adj, stats::p.adjust(combined$p_value, "bonferroni")
  )
  expect_true(all(combined$p_adjust_method == "bonferroni"))
  expected <- c(
    verbosity = "BIAS", self_preference = "no_signal",
    bandwagon = "no_signal", position = "BIAS"
  )
  expect_identical(
    rlang::set_names(combined$verdict, combined$probe), expected
  )
  bandwagon <- combined[combined$probe == "bandwagon", ]
  expect_equal(bandwagon$p_value_adj, 0.125)

  # Without a correction the constituents' own verdicts are reproduced.
  none <- combine_bias(pointwise, position, p_adjust = "none")
  expect_equal(none$p_value_adj, none$p_value)
  expect_identical(
    none$verdict[none$probe == "bandwagon"], "BIAS"
  )
  expect_identical(
    none$verdict[match(position$probe, none$probe)], position$verdict
  )
})

test_that("bias objects print a heading and the probe table", {
  res <- probe_bias(
    tibble::tibble(input = paste("answer", 1:20)),
    function(d) ifelse(nchar(d$input) > 40, "C", "P"),
    probes = c("verbosity", "bandwagon"), seed = 1, audit_id = "snapshot"
  )
  expect_snapshot(print(res))
})

test_that("combine_bias keeps only evidence for each constituent's rows", {
  pointwise <- probe_bias(
    tibble::tibble(input = paste("answer", 1:20)),
    function(d) rep("C", nrow(d)), probes = "verbosity",
    audit_id = "bundle-1", seed = 1
  )
  pairs <- pair_data()[rep(1:2, 10), ]
  always_first <- probe_position(
    pairs, function(d) rep("A", nrow(d)),
    "candidate_a", "candidate_b", context_cols = "context",
    audit_id = "bundle-1", seed = 1
  )
  consistent <- probe_position(
    pairs, function(d) ifelse(grepl("good", d$.candidate_1), "A", "B"),
    "candidate_a", "candidate_b", context_cols = "context",
    audit_id = "bundle-1", seed = 1
  )
  combined <- combine_bias(pointwise, always_first)
  # Row-subsetting keeps the attributes, including stale position evidence.
  kept <- combined[combined$probe != "position", ]
  expect_named(attr(kept, "evidence"), "position")
  again <- combine_bias(kept, consistent)
  expect_identical(names(attr(again, "evidence")), "position")
  expect_identical(names(attr(again, "item_evidence")), "position")
  expect_setequal(
    unique(attr(again, "evidence")$position$display_choice), c("A", "B")
  )
  expect_false(any(attr(again, "item_evidence")$position$flipped))
  expect_identical(again$verdict[again$probe == "position"], "no_signal")

  # Two row-subsets of one combined object recombine.
  halves <- combine_bias(
    combined[combined$probe == "verbosity", ],
    combined[combined$probe == "position", ]
  )
  expect_setequal(halves$probe, c("verbosity", "position"))
  expect_named(attr(halves, "evidence"), "position")

  # Names given to `...` do not prefix the evidence names.
  named <- combine_bias(pointwise = pointwise, position = consistent)
  expect_named(attr(named, "evidence"), "position")
  expect_error(
    probe_named_list(list(position = 1, position = 2), "position"),
    "named uniquely"
  )

  # An unnamed evidence list is refused rather than filtered down to nothing.
  unnamed <- consistent
  attr(unnamed, "evidence") <- list(attr(consistent, "evidence")$position)
  expect_error(
    combine_bias(pointwise, unnamed), "evidence attribute must be a list named"
  )
  unnamed <- consistent
  attr(unnamed, "item_evidence") <- list(1)
  expect_error(combine_bias(pointwise, unnamed), "item_evidence attribute")
})

test_that("every internally assigned position column is reserved", {
  judge <- function(d) rep("A", nrow(d))
  for (name in c(".judge_item_id", "display_choice", "canonical_choice")) {
    data <- pair_data()
    data[[name]] <- "x"
    expect_error(
      probe_position(
        data, judge, "candidate_a", "candidate_b", context_cols = name
      ),
      name, fixed = TRUE
    )
  }
})

test_that("probe_bias validates id_col on the full data before sampling", {
  data <- tibble::tibble(
    input = paste("answer", 1:6), id = c(1, 1, 2, 3, 4, 5)
  )
  judge <- function(d) rep("C", nrow(d))
  expect_error(
    probe_bias(data, judge, id_col = "id", n = 2, probes = "verbosity"),
    "unique, non-missing IDs"
  )
  data$id <- 1:6
  expect_s3_class(
    probe_bias(data, judge, id_col = "id", n = 2, probes = "verbosity"),
    "yourhonor_bias"
  )
})
