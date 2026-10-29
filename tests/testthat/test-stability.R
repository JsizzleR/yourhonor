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

test_that("the pairwise interval is the bootstrap widened to Wilson", {
  det_judge <- function(d) rep("C", nrow(d))
  data <- tibble::tibble(input = paste("answer", 1:20))
  st <- test_retest(
    data, det_judge, input_cols = "input", n_runs = 4,
    label_levels = c("I", "C"), n_boot = 50, seed = 1
  )
  wilson <- yourhonor:::wilson_interval(0, 20, 0.95)
  # The bootstrap alone is the degenerate [0, 0]; the Wilson bound wins.
  expect_equal(st$pairwise_disagreement_conf_low, 0)
  expect_equal(st$pairwise_disagreement_conf_high, wilson[[2]])
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

test_that("a perfectly stable two-class judge is boundary degenerate", {
  judge <- function(d) {
    item <- as.integer(sub("answer ", "", d$input, fixed = TRUE))
    ifelse(item %% 2L == 0L, "C", "I")
  }
  st <- test_retest(
    tibble::tibble(input = paste("answer", 1:20)), judge,
    input_cols = "input", n_runs = 3, label_levels = c("I", "C"),
    n_boot = 50, seed = 1
  )
  expect_identical(st$stability_status, "ok")
  expect_equal(st$stability, 1)
  expect_equal(st$stability_conf_low, 1)
  expect_equal(st$stability_conf_high, 1)
  expect_true(st$stability_boundary_degenerate)
  expect_false(st$stability_ci_reliable)
  expect_true(st$pairwise_ci_reliable)
  expect_false(st$ci_reliable)
})

test_that("weighted stability reports AC2 with a usable interval", {
  judge <- function(d) {
    item <- as.integer(sub("answer ", "", d$input, fixed = TRUE))
    base <- (item %% 3L) + 1L
    bump <- as.integer(runif(nrow(d)) < 0.2)
    c("I", "P", "C")[pmin(3L, base + bump)]
  }
  st <- test_retest(
    tibble::tibble(input = paste("answer", 1:40)), judge,
    input_cols = "input", n_runs = 3, label_levels = c("I", "P", "C"),
    weights = "ordinal", n_boot = 50, seed = 2
  )
  expect_identical(st$stability_status, "ok")
  expect_identical(st$stability_coefficient, "gwet_ac2")
  expect_identical(st$weights, "ordinal")
  expect_true(is.finite(st$stability))
  expect_lt(st$stability, 1)
  expect_true(st$stability_ci_reliable)
  expect_false(st$stability_boundary_degenerate)
  expect_lte(st$stability_conf_low, st$stability)
  expect_gte(st$stability_conf_high, st$stability)
})

test_that("fewer than two items with paired runs is insufficient_items", {
  one_scored <- matrix(c("C", NA, "P", NA, "C", NA), nrow = 3)
  st <- yourhonor:::summarize_stability_runs(one_scored, n_boot = 20)
  expect_identical(st$stability_status, "insufficient_items")
  expect_true(is.na(st$stability))
  expect_equal(st$n_items_scored, 0)
  expect_true(is.na(st$pairwise_disagreement))

  # An all-missing run cannot drop the run count below two once rows are
  # filtered to those with two observed values, so this is insufficient
  # items, not a separate run-count status.
  one_run <- matrix(c("C", "P", "C", NA, NA, NA), nrow = 3)
  st <- yourhonor:::summarize_stability_runs(one_run, n_boot = 20)
  expect_identical(st$stability_status, "insufficient_items")
  expect_error(
    yourhonor:::summarize_stability_runs(matrix(NA_character_, 2, 2)),
    "Every stability judgment was missing"
  )
})

test_that("chance-corrected estimate and status are computed together", {
  runs <- matrix(c("C", "P", "C", "P", "C", "P", "C", "P"), nrow = 4)
  levels <- c("C", "P")
  out <- yourhonor:::chance_corrected_stability(
    runs, "unweighted", levels, levels
  )
  expect_named(out, c("estimate", "status"))
  expect_identical(out$status, "ok")
  expect_equal(out$estimate, 1)
  flat <- yourhonor:::chance_corrected_stability(
    matrix("C", 4, 2), "unweighted", "C", "C"
  )
  expect_identical(flat$status, "insufficient_variation")
  expect_true(is.na(flat$estimate))
})

test_that("resampling the per-item vector matches the matrix statistic", {
  set.seed(3)
  runs <- matrix(sample(c("C", "P", NA), 60, replace = TRUE), nrow = 20)
  d <- yourhonor:::item_pairwise_disagreement(runs)
  idx <- sample.int(20, 20, replace = TRUE)
  expect_equal(
    yourhonor:::mean_or_na(d[idx]),
    mean(yourhonor:::item_pairwise_disagreement(runs[idx, ]), na.rm = TRUE)
  )
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
  err <- rlang::catch_cnd(test_retest(data, judge, n_runs = 1))
  expect_identical(rlang::call_name(err$call), "test_retest")
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

test_that("per-run judge keys map source rows to what the judge saw", {
  seen <- list()
  judge <- function(d) {
    seen[[length(seen) + 1L]] <<- d
    ifelse(nchar(d$input) %% 2L == 0L, "C", "P")
  }
  data <- tibble::tibble(input = c("a", "bb", "ccc", "dddd", "eeeee"))
  st <- test_retest(
    data, judge, input_cols = "input", n_runs = 3,
    label_levels = c("P", "C"), n_boot = 20, seed = 4
  )
  expect_identical(dim(st$judge_item_ids), dim(st$runs))
  for (run in seq_len(3L)) {
    key <- st$judge_item_ids[, run]
    expect_setequal(key, 1:5)
    # The key for source row i is the .item_id attached to that row's input.
    visible <- seen[[run]]
    expect_identical(visible$input[match(key, visible$.item_id)], data$input)
  }
  expect_false(all(st$judge_item_ids[, 1] == st$judge_item_ids[, 2]))
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
    yourhonor:::chance_corrected_stability(
      runs, "unweighted", c("C", "c"), c("C", "c")
    )$estimate,
    -1
  )
  identical_runs <- matrix(rep(c("C", "c", "C", "c"), 2), ncol = 2)
  expect_equal(
    yourhonor:::chance_corrected_stability(
      identical_runs, "unweighted", c("C", "c"), c("C", "c")
    )$estimate,
    1
  )
  # The inferred universe of a matrix is its cells, not its rows, and factor
  # columns contribute their labels, not their codes.
  expect_setequal(yourhonor:::observed_categories(runs), c("C", "c"))
  mixed <- data.frame(a = factor(c("yes", "no")), b = c("no", "maybe"))
  expect_setequal(yourhonor:::observed_categories(mixed), c("yes", "no", "maybe"))
  expect_equal(
    yourhonor:::summarize_stability_runs(runs, n_boot = 1)$stability, -1
  )
})

test_that("stability printing shows both intervals", {
  judge <- function(d) {
    item <- as.integer(sub("answer ", "", d$input, fixed = TRUE))
    labels <- ifelse(item %% 2L == 0L, "C", "I")
    labels[item %in% c(3L, 8L)] <- ifelse(runif(2) < 0.5, "C", "I")
    labels
  }
  st <- test_retest(
    tibble::tibble(input = paste("answer", 1:20)), judge,
    input_cols = "input", n_runs = 3, label_levels = c("I", "C"),
    n_boot = 30, seed = 1
  )
  expect_snapshot(print(st), cran = TRUE)
  flat <- test_retest(
    tibble::tibble(input = paste("answer", 1:6)), function(d) rep("C", nrow(d)),
    input_cols = "input", n_runs = 2, label_levels = c("I", "C"),
    n_boot = 30, seed = 1
  )
  expect_snapshot(print(flat), cran = TRUE)
})

test_that("inferred factor levels are unioned with observed run labels", {
  data <- tibble::tibble(input = paste("answer", 1:8))
  run <- 0
  judge <- function(d) {
    run <<- run + 1
    if (run == 1) {
      factor(rep(c("A", "B"), 4), levels = c("A", "B"))
    } else {
      rep(c("A", "C"), 4)
    }
  }
  st <- test_retest(
    data, judge, input_cols = "input", n_runs = 2, n_boot = 20, seed = 1
  )
  expect_identical(st$label_levels, c("A", "B", "C"))
})

test_that("an unused blank factor level is dropped from inferred levels", {
  data <- tibble::tibble(input = paste("answer", 1:6))
  judge <- function(d) {
    factor(rep(c("C", "P"), length.out = nrow(d)), levels = c("C", "P", " "))
  }
  st <- test_retest(
    data, judge, input_cols = "input", n_runs = 2, n_boot = 20, seed = 1
  )
  expect_identical(st$label_levels, c("C", "P"))
})
