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

test_that("paired flip test handles no discordant pairs", {
  res <- yourhonor:::paired_flip_test(c(1, 2, 3), c(1, 2, 3))
  expect_equal(res$flip_rate, 0)
  expect_equal(res$direction, "none")
  expect_equal(res$n_scored, 3)
  expect_true(is.na(res$p_value))
})

test_that("bandwagon probe catches a crowd-following judge", {
  crowd_judge <- function(d) ifelse(grepl("looks right", d$input), "C", "P")
  data <- tibble::tibble(input = paste("answer", 1:30))
  res <- probe_bias(data, crowd_judge, probes = "bandwagon", seed = 1)
  expect_equal(res$direction, "favors_perturbed")
  expect_equal(res$verdict, "BIAS")
})

test_that("p-values are corrected across pointwise probes", {
  flat_judge <- function(d) rep("C", nrow(d))
  data <- tibble::tibble(input = paste("answer", 1:20))
  res <- probe_bias(
    data, flat_judge,
    probes = c("verbosity", "self_preference", "bandwagon"), seed = 1
  )
  ok <- !is.na(res$p_value)
  expect_true(all(res$p_value_adj[ok] >= res$p_value[ok] - 1e-9))
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
  expect_identical(
    seen, c(".item_id", "context", ".candidate_1", ".candidate_2")
  )
  expect_false(any(c(".condition", ".repeat", ".display_id") %in% seen))
  evidence <- attr(res, "evidence")
  original <- evidence[evidence$.condition == "original", ]
  expect_true(any(grepl("good\nmultiline answer", original$candidate_a, fixed = TRUE)))
  expect_true(all(original$canonical_choice %in% c("candidate_a", "candidate_b")))
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
  expect_gt(res$n_missing, 0)
  expect_equal(res$directional_rate, 1)
  expect_equal(res$n_directional_items, res$n_scored)
})
