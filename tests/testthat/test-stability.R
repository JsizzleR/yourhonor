test_that("a single-class deterministic judge has bounded disagreement but undefined AC1", {
  det_judge <- function(d) rep("C", nrow(d))
  data <- tibble::tibble(input = paste("answer", 1:20))
  st <- test_retest(
    data, det_judge, input_cols = "input", n_runs = 4,
    label_levels = c("I", "P", "C"),
    n_boot = 50, seed = 1
  )
  expect_s3_class(st, "yourhonor_stability")
  expect_equal(st$any_flip_rate_at_n_runs, 0)
  expect_equal(st$pairwise_disagreement, 0)
  expect_gt(st$pairwise_disagreement_conf_high, 0)
  expect_equal(st$percent_agreement, 1)
  expect_true(st$pairwise_ci_reliable)
  expect_true(is.na(st$stability))
  expect_identical(st$stability_status, "insufficient_variation")
  expect_false(st$stability_ci_reliable)
  expect_equal(st$stability_n_boot_effective, 0)
  expect_equal(st$missing_rate, 0)
  expect_equal(nrow(st$per_item), 20)
})

test_that("all-missing items do not invalidate otherwise usable Gwet stability", {
  judge <- function(d) {
    item <- as.integer(sub("answer ", "", d$input, fixed = TRUE))
    labels <- ifelse(item %% 2L == 1L, "I", "C")
    labels[item == 100L] <- NA_character_
    labels
  }
  st <- test_retest(
    tibble::tibble(input = paste("answer", 1:100)), judge,
    input_cols = "input", n_runs = 3, label_levels = c("I", "C"),
    n_boot = 50, seed = 1
  )
  expect_equal(st$n_items_scored, 99)
  expect_equal(st$pairwise_disagreement, 0)
  expect_true(is.finite(st$stability))
  expect_true(st$pairwise_ci_reliable)
})

test_that("chance-corrected stability is low for a noisy judge", {
  noisy_judge <- function(d) sample(c("C", "P"), nrow(d), replace = TRUE)
  data <- tibble::tibble(input = paste("answer", 1:80))
  st <- test_retest(
    data, noisy_judge, input_cols = "input", n_runs = 5,
    n_boot = 100, seed = 1
  )
  expect_gt(st$pairwise_disagreement, 0)
  expect_lt(st$stability, st$percent_agreement)
  expect_lt(st$stability, 0.3)
})

test_that("any-flip rate depends on run count but pairwise disagreement does not", {
  judge <- function(d) ifelse(runif(nrow(d)) < 0.9, "C", "P")
  data <- tibble::tibble(input = paste("answer", 1:2000))
  two <- test_retest(
    data, judge, input_cols = "input", n_runs = 2, n_boot = 50, seed = 10
  )
  ten <- test_retest(
    data, judge, input_cols = "input", n_runs = 10, n_boot = 50, seed = 10
  )
  expect_gt(ten$any_flip_rate_at_n_runs, two$any_flip_rate_at_n_runs)
  expect_lt(abs(ten$pairwise_disagreement - two$pairwise_disagreement), 0.03)
})

test_that("missing grades are counted and not treated as a label", {
  calls <- 0L
  judge <- function(d) {
    calls <<- calls + 1L
    if (calls == 1L) c("C", NA) else c("C", "C")
  }
  st <- test_retest(
    tibble::tibble(input = c("x", "y")), judge,
    input_cols = "input", n_runs = 2, label_levels = c("I", "C"),
    n_boot = 50, seed = 1
  )
  expect_equal(st$missing_rate, 0.25)
  expect_true(is.na(st$per_item$flipped[[2]]))
  expect_equal(st$per_item$n_missing[[2]], 1)
  expect_equal(st$pairwise_disagreement, 0)
  expect_true(is.na(st$stability))
})

test_that("invalid run counts and labels error", {
  data <- tibble::tibble(input = "x")
  judge <- function(d) "C"
  expect_error(test_retest(data, judge, n_runs = 1), "n_runs")
  expect_error(test_retest(data, judge, n_runs = 0), "n_runs")
  expect_error(
    test_retest(data, function(d) "garbage", n_runs = 2,
                input_cols = "input", label_levels = c("I", "C")),
    "outside"
  )
  expect_error(
    test_retest(data, judge, n_runs = 2, weights = "ordinal"),
    "label_levels"
  )
  expect_error(
    test_retest(
      data, judge, input_cols = "input", n_runs = 2,
      label_levels = c("C", "C")
    ),
    "unique"
  )
})

test_that("stability grading is redacted, opaque, and keyed", {
  data <- tibble::tibble(
    source_id = paste0(c("LABEL-C", "LABEL-P", "LABEL-C", "LABEL-P"), 1:4),
    input = paste("answer", 1:4),
    human_label = c("C", "P", "C", "P"),
    judge_label = c("P", "C", "P", "C")
  )
  seen_names <- NULL
  seen_ids <- NULL
  judge <- function(d) {
    seen_names <<- names(d)
    seen_ids <<- d$.item_id
    item <- as.integer(sub("answer ", "", d$input, fixed = TRUE))
    out <- tibble::tibble(
      .item_id = d$.item_id,
      label = ifelse(item %% 2L == 1L, "C", "P")
    )
    out[nrow(out):1, ]
  }
  st <- test_retest(
    data, judge, input_cols = "input", id_col = "source_id", n_runs = 2,
    label_levels = c("P", "C"), n_boot = 50, seed = 1
  )
  expect_identical(seen_names, c(".item_id", "input"))
  expect_identical(seen_ids, 1:4)
  expect_identical(st$runs[, 1], c("C", "P", "C", "P"))
  expect_false(any(c("human_label", "judge_label", "source_id") %in% seen_names))
})

test_that("ties produce an explicit NA mode", {
  expect_true(is.na(yourhonor:::modal_value(c("C", "P"))))
  expect_false(is.na(yourhonor:::modal_value(c("C", "C", "P"))))
  expect_true(yourhonor:::is_tie(c("C", "P")))
  expect_false(yourhonor:::is_tie(c("C", "C", "P")))
})

test_that("stability keeps case-variant labels distinct", {
  runs <- matrix(
    c("C", "c", "C", "c", "C", "c", "c", "C", "c", "C", "c", "C"),
    ncol = 2
  )
  expect_equal(
    yourhonor:::chance_corrected_stability(runs, "unweighted", c("C", "c")),
    -1
  )
  identical_runs <- matrix(rep(c("C", "c", "C", "c"), 2), ncol = 2)
  expect_equal(
    yourhonor:::chance_corrected_stability(identical_runs, "unweighted", c("C", "c")),
    1
  )
  # The inferred universe of a matrix is its cells, not its rows, and factor
  # columns contribute their labels, not their codes.
  expect_setequal(yourhonor:::observed_categories(runs), c("C", "c"))
  mixed <- data.frame(a = factor(c("yes", "no")), b = c("no", "maybe"))
  expect_setequal(yourhonor:::observed_categories(mixed), c("yes", "no", "maybe"))
  expect_equal(yourhonor:::chance_corrected_stability(runs, "unweighted", NULL), -1)
})
