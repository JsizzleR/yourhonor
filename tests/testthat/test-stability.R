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

test_that("linear and quadratic stability is AC2, as agreement() and irrCAC score it", {
  levels <- c("1", "2", "3", "4")
  data <- tibble::tibble(input = paste("answer", 1:40))
  # Every fifth item flips up one category on half its runs: near misses
  # only, which a weighted coefficient credits in part.
  judge <- function(d) {
    item <- as.integer(sub("answer ", "", d$input, fixed = TRUE))
    base <- (item %% 4L) + 1L
    flip <- item %% 5L == 0L & runif(nrow(d)) < 0.5
    as.character(pmin(base + flip, 4L))
  }
  runs <- function(weights) {
    test_retest(
      data, judge, input_cols = "input", n_runs = 3, label_levels = levels,
      weights = weights, n_boot = 50, seed = 3
    )
  }
  unweighted <- runs("unweighted")
  estimates <- c(unweighted = unweighted$stability)
  for (weights in c("linear", "quadratic")) {
    st <- runs(weights)
    expect_identical(st$weights, weights)
    expect_identical(st$stability_coefficient, "gwet_ac2")
    expect_identical(st$stability_status, "ok")
    # The same seed draws the same runs; only the coefficient changes.
    expect_identical(st$runs, unweighted$runs)
    expect_identical(
      st$pairwise_disagreement, unweighted$pairwise_disagreement
    )
    rated <- as.data.frame(st$runs)
    ag <- agreement(
      rated, rater_cols = names(rated), coefficients = "gwet_ac2",
      label_levels = levels, weights = weights, n_boot = 30
    )
    expect_equal(st$stability, ag$estimate, info = weights)
    oracle <- irrCAC::gwet.ac1.raw(
      rated, weights = weights, categ.labels = levels
    )$est$coeff.val
    expect_equal(st$stability, oracle, tolerance = 1e-5, info = weights)
    expect_lte(st$stability_conf_low, st$stability)
    expect_gte(st$stability_conf_high, st$stability)
    estimates[[weights]] <- st$stability
  }
  # Quadratic weights credit a one-step miss more than linear ones do.
  expect_lt(estimates[["unweighted"]], estimates[["linear"]])
  expect_lt(estimates[["linear"]], estimates[["quadratic"]])
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
    class = "yourhonor_input_error"
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
  withr::local_seed(3)
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
  expect_error(
    test_retest(data, judge, n_runs = 1), "n_runs",
    class = "yourhonor_input_error"
  )
  expect_error(
    test_retest(data, judge, n_runs = 0), "n_runs",
    class = "yourhonor_input_error"
  )
  expect_error(
    test_retest(data, function(d) "garbage", n_runs = 2,
                input_cols = "input", label_levels = c("I", "C")),
    class = "yourhonor_judge_error"
  )
  expect_error(
    test_retest(data, judge, n_runs = 2, weights = "ordinal"),
    "label_levels", class = "yourhonor_input_error"
  )
  expect_error(
    test_retest(
      data, judge, input_cols = "input", n_runs = 2,
      label_levels = c("C", "C")
    ),
    class = "yourhonor_input_error"
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
  expect_none(c("human_label", "judge_label", "source_id") %in% seen_names)
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
  expect_snapshot(print(st))
  flat <- test_retest(
    tibble::tibble(input = paste("answer", 1:6)), function(d) rep("C", nrow(d)),
    input_cols = "input", n_runs = 2, label_levels = c("I", "C"),
    n_boot = 30, seed = 1
  )
  expect_snapshot(print(flat))
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

test_that("the worst-case disagreement counts every pair with a missing judgment", {
  runs <- rbind(
    c("A", "A", "A"),    # complete, agreeing: 0
    c("A", "B", "A"),    # complete: 2 of 3 pairs disagree
    c("A", "A", NA),     # 1 observed pair agrees, 2 pairs involve NA
    c("A", NA, NA),      # one judgment: every pair involves NA
    c(NA, NA, NA)        # nothing observed
  )
  st <- withr::with_seed(1, yourhonor:::summarize_stability_runs(runs, n_boot = 50))
  expect_equal(
    st$per_item$pairwise_disagreement_worst_case,
    c(0, 2 / 3, 2 / 3, 1, 1)
  )
  expect_equal(st$pairwise_disagreement_worst_case, (0 + 2 / 3 + 2 / 3 + 1 + 1) / 5)
  expect_equal(st$n_items_scored, 3)
  expect_gte(
    st$pairwise_disagreement_worst_case_high,
    st$pairwise_disagreement_conf_high
  )
  expect_lte(st$pairwise_disagreement_worst_case_high, 1)
})

test_that("the worst-case bound stays finite at an extreme confidence level", {
  runs <- cbind(c(rep("A", 30), "B"), c(rep("A", 29), NA, "A"))
  st <- withr::with_seed(1, yourhonor:::summarize_stability_runs(
    runs, n_boot = 50, conf_level = 1 - .Machine$double.eps / 2
  ))
  expect_true(is.finite(st$pairwise_disagreement_conf_high))
  expect_true(is.finite(st$pairwise_disagreement_worst_case_high))
})

test_that("the any-flip rate is over complete items, with a worst-case companion", {
  runs <- rbind(
    c("A", "A", "A"),
    c("A", "B", "A"),
    c("A", "A", NA),     # no observed flip, but incomplete
    c("A", "B", NA),     # an observed flip, incomplete
    c("B", "B", "B")
  )
  st <- withr::with_seed(1, yourhonor:::summarize_stability_runs(runs, n_boot = 50))
  expect_equal(st$n_items_complete, 3)
  expect_equal(st$any_flip_rate_at_n_runs, 1 / 3)
  expect_equal(st$any_flip_rate_worst_case, 3 / 5)
  none <- withr::with_seed(1, yourhonor:::summarize_stability_runs(
    rbind(c("A", NA), c(NA, "B"), c("A", NA)), n_boot = 20
  ))
  expect_equal(none$n_items_complete, 0)
  expect_true(is.na(none$any_flip_rate_at_n_runs))
  expect_equal(none$any_flip_rate_worst_case, 1)
})

test_that("an AC1 that is not estimable reports no interval", {
  runs <- matrix(c("C", "P", "C", NA, NA, NA, "P", NA), nrow = 4, ncol = 2)
  st <- withr::with_seed(1, yourhonor:::summarize_stability_runs(runs, n_boot = 200))
  expect_identical(st$stability_status, "insufficient_items")
  expect_true(is.na(st$stability))
  expect_true(is.na(st$stability_conf_low))
  expect_true(is.na(st$stability_conf_high))
  expect_false(st$stability_ci_reliable)
  expect_false(st$ci_reliable)
})

test_that("the pre-release flip_rate alias is gone", {
  st <- withr::with_seed(1, yourhonor:::summarize_stability_runs(
    rbind(c("A", "A"), c("A", "B")), n_boot = 20
  ))
  expect_false("flip_rate" %in% names(st))
  expect_true("any_flip_rate_at_n_runs" %in% names(st))
})

# A stability object's bounds and counts rebuilt from its own runs and
# bootstrap state (verify_stability_evidence()), and what refuses a rebuild.
random_stability_runs <- function(i) {
  withr::with_seed(i, {
    n <- sample(c(1:6, 40, 150), 1)
    r <- sample(2:5, 1)
    runs <- matrix(
      sample(c("A", "B", "C"), n * r, TRUE, prob = c(0.7, 0.2, 0.1)), n, r
    )
    holes <- sample(c(0, 0.05, 0.3), 1)
    runs[stats::runif(n * r) < holes] <- NA
    if (all(is.na(runs))) runs[1, 1] <- "A"
    runs
  })
}

test_that("stored stability bounds rebuild bit for bit from runs and state", {
  for (i in 1:40) {
    runs <- random_stability_runs(i)
    n_boot <- c(0, 1, 37, 200)[[i %% 4 + 1]]
    weights <- if (i %% 3 == 0) "ordinal" else "unweighted"
    levels <- if (weights == "ordinal") c("A", "B", "C") else NULL
    st <- withr::with_seed(i, summarize_stability_runs(
      runs, n_boot = n_boot, weights = weights, label_levels = levels,
      conf_level = c(0.9, 0.95)[[i %% 2 + 1]]
    ))
    verified <- verify_stability_evidence(st)
    expect_null(verified$reason, label = paste("case", i))
    expect_identical(verified$stability, st, label = paste("case", i))
    # A saved and reloaded object is the same evidence.
    path <- withr::local_tempfile(fileext = ".rds")
    saveRDS(st, path)
    expect_identical(
      verify_stability_evidence(readRDS(path))$stability, st,
      label = paste("case", i)
    )
  }
  # Evidence whose bootstrap ran in several chunks replays too.
  big <- matrix("A", 3000, 2)
  big[1:150, 2] <- "B"
  big[151:200, 2] <- NA
  expect_gt(400, bootstrap_chunk_cells %/% nrow(big))
  st <- withr::with_seed(3, summarize_stability_runs(big, n_boot = 400))
  expect_identical(verify_stability_evidence(st)$stability, st)
  # Chunking moves no draw in the replay either.
  st <- withr::with_seed(5, summarize_stability_runs(
    random_stability_runs(7), n_boot = 61
  ))
  replay <- function(chunk_cells) {
    runs <- st$runs
    d <- item_pairwise_disagreement(runs)
    with_rng_state(st$bootstrap_seed, pairwise_bootstrap(
      d, item_worst_case_disagreement(runs, d),
      item_best_case_disagreement(runs), 61L, st$conf_level,
      chunk_cells = chunk_cells
    ))
  }
  expect_identical(replay(7L * nrow(st$runs)), replay(bootstrap_chunk_cells))
  expect_identical(
    replay(1L)$pairwise$conf_high, st$pairwise_disagreement_conf_high
  )
})

test_that("evidence from test_retest() and stability_vitals() rebuilds", {
  judge <- function(d) sample(c("C", "P", NA), nrow(d), replace = TRUE)
  st <- test_retest(
    tibble::tibble(input = paste("answer", 1:30)), judge,
    input_cols = "input", n_runs = 4, n_boot = 80, seed = 3, audit_id = "r"
  )
  expect_identical(verify_stability_evidence(st)$stability, st)
  rows <- expand.grid(id = sprintf("i%02d", 1:25), epoch = 1:3,
                      stringsAsFactors = FALSE)
  rows$input <- paste("q", rows$id)
  rows$score <- withr::with_seed(2, sample(c("C", "P", NA), nrow(rows), TRUE))
  vt <- stability_vitals(rows, content_cols = "input", n_boot = 80, seed = 4)
  expect_identical(verify_stability_evidence(vt)$stability, vt)
  # Without a seed there is still a state, and it replays.
  free <- stability_vitals(rows, content_cols = "input", n_boot = 80)
  expect_identical(verify_stability_evidence(free)$stability, free)
})

test_that("bootstrap_seed is the state the item bootstrap starts from", {
  runs <- random_stability_runs(3)
  st <- withr::with_seed(9, summarize_stability_runs(runs, n_boot = 20))
  expect_identical(st$bootstrap_seed, withr::with_seed(9, .Random.seed))
  # It follows the schema, so an object without it still has every field.
  expect_identical(names(st)[seq_along(stability_fields)], stability_fields)
  expect_identical(names(st)[[length(stability_fields) + 1L]], "bootstrap_seed")
  # No draws, no state, and nothing about the caller's state is touched.
  withr::with_seed(9, {
    before <- .Random.seed
    none <- summarize_stability_runs(runs, n_boot = 0)
    expect_identical(.Random.seed, before)
  })
  expect_true("bootstrap_seed" %in% names(none))
  expect_null(none$bootstrap_seed)
  withr::with_preserve_seed({
    if (exists(".Random.seed", envir = globalenv())) {
      rm(".Random.seed", envir = globalenv())
    }
    summarize_stability_runs(runs, n_boot = 0)
    expect_false(exists(".Random.seed", envir = globalenv()))
  })
  # Keeping the state draws nothing: the caller's stream after the call is
  # the stream of the same draws made directly.
  after <- withr::with_seed(9, {
    summarize_stability_runs(runs, n_boot = 20)
    stats::runif(3)
  })
  direct <- withr::with_seed(9, {
    for (i in 1:20) sample.int(nrow(runs), nrow(runs), replace = TRUE)
    stats::runif(3)
  })
  expect_identical(after, direct)
  # A generator kind other than the default is kept with its state.
  lec <- withr::with_seed(
    9, summarize_stability_runs(runs, n_boot = 20),
    .rng_kind = "L'Ecuyer-CMRG"
  )
  expect_identical(verify_stability_evidence(lec)$stability, lec)
})

test_that("verifying stability evidence leaves the caller's random state alone", {
  runs <- random_stability_runs(11)
  lec <- withr::with_seed(
    2, summarize_stability_runs(runs, n_boot = 30),
    .rng_kind = "L'Ecuyer-CMRG"
  )
  withr::with_seed(5, {
    before <- .Random.seed
    expect_null(verify_stability_evidence(lec)$reason)
    expect_identical(.Random.seed, before)
    expect_identical(RNGkind()[[1]], "Mersenne-Twister")
  })
  # With no state before, none after, and the generator kinds unchanged.
  withr::with_preserve_seed({
    kinds <- RNGkind()
    if (exists(".Random.seed", envir = globalenv())) {
      rm(".Random.seed", envir = globalenv())
    }
    expect_null(verify_stability_evidence(lec)$reason)
    expect_false(exists(".Random.seed", envir = globalenv()))
    expect_identical(RNGkind(), kinds)
  })
})

# Two runs, with holes: the bootstrap, not the Wilson interval, sets the
# observed lower bound and both case bounds, so the draws matter.
bootstrap_bound_runs <- function() {
  runs <- cbind(rep("A", 60), c(rep("B", 12), rep("A", 48)))
  runs[13:18, 2] <- NA
  runs
}

test_that("a state with a payload word that reads as NA still replays", {
  # A 32-bit word of the generator state equal to -2^31 is NA_integer_ in R;
  # only the first element, the kind code, can never be NA.
  state <- withr::with_seed(4, .Random.seed)
  state[[100]] <- NA_integer_
  runs <- bootstrap_bound_runs()
  st <- withr::with_preserve_seed({
    assign(".Random.seed", state, envir = globalenv())
    summarize_stability_runs(runs, n_boot = 50)
  })
  expect_identical(st$bootstrap_seed, state)
  verified <- verify_stability_evidence(st)
  expect_null(verified$reason)
  expect_identical(verified$stability, st)
})

test_that("a warning raised during a genuine rebuild is not a refusal", {
  # The byte compiler, for one, can warn while the replay runs; that says
  # nothing about the evidence, so it reaches the caller and the evidence
  # still verifies.
  runs <- bootstrap_bound_runs()
  st <- withr::with_seed(1, summarize_stability_runs(runs, n_boot = 100))
  replay <- pairwise_bootstrap
  local_mocked_bindings(pairwise_bootstrap = function(...) {
    warning("a note from the compiler")
    replay(...)
  })
  expect_warning(verified <- verify_stability_evidence(st), "compiler")
  expect_null(verified$reason)
  expect_identical(verified$stability, st)
})

test_that("only an object of class yourhonor_stability alone is rebuilt", {
  st <- withr::with_seed(1, summarize_stability_runs(
    bootstrap_bound_runs(), n_boot = 100
  ))
  for (cls in list(c("yourhonor_stability", "data.frame"),
                   c("other", "yourhonor_stability"))) {
    bad <- st
    class(bad) <- cls
    expect_no_error(reason <- verify_stability_evidence(bad)$reason)
    expect_identical(
      reason, "Stability evidence cannot be rebuilt from its runs."
    )
  }
})

test_that("a state R refuses leaves a session without a state as it was", {
  runs <- bootstrap_bound_runs()
  st <- withr::with_seed(1, summarize_stability_runs(runs, n_boot = 100))
  for (state in list(c(10403L, 1:5), st$bootstrap_seed[1:2])) {
    bad <- st
    bad$bootstrap_seed <- state
    withr::with_preserve_seed({
      kinds <- RNGkind()
      if (exists(".Random.seed", envir = globalenv())) {
        rm(".Random.seed", envir = globalenv())
      }
      expect_identical(
        verify_stability_evidence(bad)$reason,
        "Stability evidence cannot be rebuilt from its runs."
      )
      expect_false(exists(".Random.seed", envir = globalenv()))
      expect_identical(RNGkind(), kinds)
      expect_no_error(stats::runif(1))
    })
  }
})

test_that("an edited stability field does not match its rebuild", {
  runs <- bootstrap_bound_runs()
  st <- withr::with_seed(1, summarize_stability_runs(runs, n_boot = 100))
  other <- withr::with_seed(2, summarize_stability_runs(runs, n_boot = 100))
  for (field in c("pairwise_disagreement_conf_low",
                  "pairwise_disagreement_worst_case_high",
                  "pairwise_disagreement_best_case_low")) {
    expect_false(identical(st[[field]], other[[field]]), label = field)
  }
  expect_null(verify_stability_evidence(st)$reason)
  mismatch <- "Stored stability bounds or counts do not match their rebuild from its runs."
  edits <- list(
    pairwise_disagreement_conf_low = st$pairwise_disagreement_conf_low + 1e-6,
    pairwise_disagreement_conf_high = st$pairwise_disagreement_conf_high - 1e-6,
    pairwise_disagreement_worst_case_high = st$pairwise_disagreement_conf_high,
    pairwise_disagreement_best_case_low = st$pairwise_disagreement_conf_low,
    pairwise_ci_reliable = !st$pairwise_ci_reliable,
    n_items = st$n_items + 1L,
    n_runs = st$n_runs + 1L,
    n_items_scored = st$n_items_scored + 1L,
    missing_rate = 0
  )
  for (field in names(edits)) {
    bad <- st
    bad[[field]] <- edits[[field]]
    expect_identical(
      verify_stability_evidence(bad)$reason, mismatch, label = field
    )
    for (value in list(NULL, NA_real_, NaN, "0.1", c(0.1, 0.2))) {
      bad <- st
      bad[field] <- list(value)
      expect_identical(
        verify_stability_evidence(bad)$reason, mismatch, label = field
      )
    }
  }
  # An input edited alone changes the rebuild, so the stored bounds no
  # longer match.
  for (field in c("n_boot", "conf_level")) {
    bad <- st
    bad[[field]] <- if (field == "n_boot") 99 else 0.9
    expect_identical(verify_stability_evidence(bad)$reason, mismatch)
  }
  for (state in list(other$bootstrap_seed, 1:3)) {
    bad <- st
    bad$bootstrap_seed <- state
    expect_identical(verify_stability_evidence(bad)$reason, mismatch)
  }
  bad <- st
  bad$runs[1, 1] <- if (identical(st$runs[1, 1], "B")) "A" else "B"
  expect_identical(verify_stability_evidence(bad)$reason, mismatch)
  # A difference inside the platform tolerance matches; the rebuilt value
  # is the one returned.
  near <- st
  near$pairwise_disagreement_conf_high <- st$pairwise_disagreement_conf_high +
    1e-12
  verified <- verify_stability_evidence(near)
  expect_null(verified$reason)
  expect_identical(
    verified$stability$pairwise_disagreement_conf_high,
    st$pairwise_disagreement_conf_high
  )
})

test_that("evidence that cannot be rebuilt says so", {
  runs <- bootstrap_bound_runs()
  st <- withr::with_seed(1, summarize_stability_runs(runs, n_boot = 100))
  cannot <- "Stability evidence cannot be rebuilt from its runs."
  inputs <- list(
    runs = list(NULL, unclass(as.data.frame(runs)), matrix(1, 3, 2),
                matrix("A", 3, 1), matrix(NA_character_, 3, 2)),
    n_boot = list(NULL, NA_real_, -1, 1.5, "100"),
    conf_level = list(NULL, 0, 1, NA_real_),
    # A state that is not a whole integer vector is refused here, and R
    # refuses one of the wrong length for its generator kind; one R accepts
    # is a state, and replays (or fails to match) like any other.
    bootstrap_seed = list(NULL, integer(0), c(st$bootstrap_seed[1:3], NA),
                          as.numeric(st$bootstrap_seed), c(10403L, 1:5))
  )
  for (field in names(inputs)) {
    for (value in inputs[[field]]) {
      bad <- st
      bad[field] <- list(value)
      expect_identical(
        verify_stability_evidence(bad)$reason, cannot, label = field
      )
    }
  }
  # An n_boot past the integer range cannot be rebuilt, and says so without
  # a coercion warning (an error under options(warn = 2)).
  for (value in c(1e12, .Machine$integer.max + 1)) {
    bad <- st
    bad$n_boot <- value
    expect_no_warning(reason <- verify_stability_evidence(bad)$reason)
    expect_identical(reason, cannot)
  }
  # A classed input, whose methods would dispatch, is refused, never an
  # error out of the policy; so is a state too short for any generator R
  # replays (R would reseed it from the clock).
  classed <- list(
    n_boot = structure(100, class = "numeric_version"),
    n_boot = stats::ts(100),
    conf_level = data.frame(x = 0.95),
    conf_level = structure(0.95, class = "numeric_version"),
    runs = structure(st$runs, class = "dist"),
    bootstrap_seed = st$bootstrap_seed[1]
  )
  for (i in seq_along(classed)) {
    bad <- st
    bad[names(classed)[[i]]] <- list(classed[[i]])
    expect_no_error(reason <- verify_stability_evidence(bad)$reason)
    expect_identical(reason, cannot, label = names(classed)[[i]])
  }
  # Nor does a classed stored field, or a run matrix the rebuild cannot
  # score, make the check raise an error.
  for (field in c("pairwise_disagreement_conf_low", "n_items", "missing_rate")) {
    bad <- st
    bad[[field]] <- structure(bad[[field]], class = "POSIXlt")
    expect_no_error(reason <- verify_stability_evidence(bad)$reason)
    expect_false(is.null(reason))
  }
  wide <- st
  wide$runs <- matrix("C", 1, 70000)
  expect_no_condition(reason <- verify_stability_evidence(wide)$reason)
  expect_identical(reason, cannot)
  # An object from before the state was kept has no bootstrap_seed field.
  old <- st
  old$bootstrap_seed <- NULL
  expect_false("bootstrap_seed" %in% names(old))
  expect_identical(verify_stability_evidence(old)$reason, cannot)
  # With no draws there is nothing to replay, so no state is needed.
  none <- withr::with_seed(1, summarize_stability_runs(runs, n_boot = 0))
  none$bootstrap_seed <- NULL
  expect_identical(
    verify_stability_evidence(none)$stability[stability_fields],
    unclass(none)[stability_fields]
  )
})
