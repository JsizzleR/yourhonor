# Regression tests for the calibration edge cases reported in issue #1.

edge_policy <- function(..., min_agreement_lower = 0.8) {
  acceptance_policy(
    min_agreement_lower = min_agreement_lower, min_n = 20, min_class_n = 1, min_recall = 0,
    max_missing_rate = 0.5, max_pairwise_disagreement = NULL, ...
  )
}

test_that("a single-class reference is insufficient_reference_variation", {
  gold <- tibble::tibble(
    human_label = rep("a", 25),
    judge_label = c(rep("a", 23), "b", "c")
  )
  cal <- calibrate_judge(
    gold, label_levels = c("a", "b", "c"), n_boot = 200, seed = 1
  )
  expect_true(all(cal$agreement$status == "insufficient_reference_variation"))
  expect_true(all(is.na(cal$agreement$estimate)))
  expect_true(all(is.na(cal$agreement$interpretation)))
  expect_false(any(cal$agreement$ci_reliable))
  # The same table re-derived through agreement() keeps the reference rule.
  expect_true(all(
    agreement(cal, n_boot = 50)$status == "insufficient_reference_variation"
  ))
  checks <- report(cal, policy = edge_policy())$checks
  expect_identical(
    checks$status[checks$check == "agreement_gwet_ac1"], "INDETERMINATE"
  )
})

test_that("a perfect sample is assessed no worse than one error", {
  human <- rep(c("y", "n"), 30)
  perfect <- calibrate_judge(
    tibble::tibble(human_label = human, judge_label = human),
    n_boot = 200, seed = 1
  )
  one_error <- calibrate_judge(
    tibble::tibble(human_label = human, judge_label = replace(human, 1, "n")),
    n_boot = 200, seed = 1
  )
  ac1 <- function(cal) cal$agreement[cal$agreement$coefficient == "gwet_ac1", ]
  expect_true(ac1(perfect)$boundary_degenerate)
  # Clopper-Pearson lower bound on 60/60 agreement, mapped with pe = 0.5.
  expect_equal(
    ac1(perfect)$conf_low_exact, (0.025^(1 / 60) - 0.5) / 0.5,
    tolerance = 1e-8
  )
  policy <- edge_policy()
  assessed <- function(cal) {
    checks <- report(cal, policy = policy)$checks
    checks[checks$check == "agreement_gwet_ac1", ]
  }
  expect_identical(assessed(perfect)$status, "PASS")
  expect_identical(assessed(one_error)$status, "PASS")
  expect_gt(assessed(perfect)$value, assessed(one_error)$value)
})

test_that("policies assess the exact bound when it is below the bootstrap", {
  human <- rep(c("y", "n"), 15)
  cal <- calibrate_judge(
    tibble::tibble(human_label = human, judge_label = replace(human, 1, "n")),
    coefficients = "cohen_kappa", n_boot = 500, seed = 1
  )
  row <- cal$agreement
  expect_lt(row$conf_low_exact, row$conf_low)
  checks <- report(cal, policy = edge_policy(coefficient = "cohen_kappa"))$checks
  check <- checks[checks$check == "agreement_cohen_kappa", ]
  expect_equal(check$value, row$conf_low_exact)
  expect_match(check$reason, "exact bound")
})

test_that("unused declared levels do not move Gwet's AC1 or AC2", {
  ratings <- data.frame(
    human = c("a", "a", "a", "b", "b", "a", "b", "a", "a", "b"),
    judge = c("a", "a", "b", "b", "b", "a", "b", "a", "a", "a")
  )
  estimate <- function(...) {
    agreement(ratings, n_boot = 20, seed = 1, ...)$estimate
  }
  expect_equal(estimate(), estimate(label_levels = c("a", "b", "c")))
  expect_equal(
    estimate(label_levels = c("a", "b")),
    estimate(label_levels = c("c", "a", "b", "d"))
  )
  ordinal <- data.frame(
    human = c("1", "2", "2", "4", "4", "1", "2", "4"),
    judge = c("1", "2", "4", "4", "2", "1", "2", "4")
  )
  ac2 <- function(levels) {
    agreement(
      ordinal, n_boot = 20, seed = 1, label_levels = levels,
      weights = "linear", coefficients = "gwet_ac2"
    )$estimate
  }
  # Weights come from the full declared scale, so an unused level still sets
  # the distances; only the chance term leaves it out.
  levels <- c("1", "2", "3", "4", "5")
  observed <- c(1L, 2L, 4L)
  expected <- irrCAC::gwet.ac1.raw(
    encode_ratings(ordinal, levels),
    weights = irrCAC::linear.weights(1:5)[observed, observed],
    categ.labels = observed
  )$est$coeff.val
  expect_equal(ac2(levels), as.numeric(expected))
  expect_false(isTRUE(all.equal(ac2(c("1", "2", "3", "4")), ac2(c("1", "2", "4")))))
})

test_that("judge abstentions are reported apart from human gaps", {
  gold <- tibble::tibble(
    human_label = c(rep(c("a", "b"), 10), NA, NA),
    judge_label = c(NA, NA, NA, rep(c("b", "a"), 1), rep(c("a", "b"), 7), "a", "a", "b")
  )
  cal <- suppressWarnings(calibrate_judge(gold, n_boot = 50, seed = 1))
  expect_identical(cal$n_judge_abstentions, 3L)
  expect_equal(cal$judge_abstention_rate, 3 / 20)
  ci <- stats::binom.test(3, 20)$conf.int
  expect_equal(
    c(cal$judge_abstention_conf_low, cal$judge_abstention_conf_high),
    as.numeric(ci)
  )
  expect_identical(cal$n_excluded, 5L)

  checks <- report(cal, policy = edge_policy(max_abstention_rate = 0.6))$checks
  expect_identical(
    checks$status[checks$check == "judge_abstention_rate"], "PASS"
  )
  checks <- report(cal, policy = edge_policy(max_abstention_rate = 0.01))$checks
  expect_identical(
    checks$status[checks$check == "judge_abstention_rate"], "FAIL"
  )
  expect_false("judge_abstention_rate" %in% report(cal, policy = edge_policy())$checks$check)
})

test_that("the default policy coefficient follows the calibration's weights", {
  levels <- c("1", "2", "3")
  gold <- tibble::tibble(
    human_label = rep(levels, 20),
    judge_label = rep(levels, 20)
  )
  ordinal <- calibrate_judge(
    gold, label_levels = levels, scale = "ordinal", n_boot = 100, seed = 1
  )
  checks <- report(ordinal, policy = edge_policy())$checks
  expect_true("agreement_gwet_ac2" %in% checks$check)
  expect_false("NOT_ASSESSED" %in% checks$status)

  nominal <- calibrate_judge(gold, label_levels = levels, n_boot = 100, seed = 1)
  expect_true(
    "agreement_gwet_ac1" %in% report(nominal, policy = edge_policy())$checks$check
  )
  expect_error(
    report(ordinal, policy = edge_policy(coefficient = "gwet_ac1")),
    "did not compute"
  )
})

test_that("the JSON artifact carries confusion counts and per-rater agreement", {
  skip_if_not_installed("jsonlite")
  gold <- tibble::tibble(
    r1 = c("a", "a", "b", "b", "a", "b", "a", "b"),
    r2 = c("a", "a", "b", "b", "a", "b", "b", "b"),
    judge_label = c("a", "b", "b", "b", "a", "b", "a", "a")
  )
  cal <- suppressWarnings(calibrate_judge(
    gold, human_cols = c("r1", "r2"), n_boot = 50, seed = 1
  ))
  path <- withr::local_tempfile(fileext = ".json")
  write_audit(cal, path)
  raw <- paste(readLines(path), collapse = "\n")
  expect_match(raw, '"confusion": \\[')
  expect_match(raw, '"per_rater_agreement": \\[')
  expect_match(raw, '"n_judge_abstentions": 0')

  parsed <- jsonlite::fromJSON(path)
  confusion <- parsed$calibration$confusion
  expect_identical(nrow(confusion), 4L)
  expect_identical(sum(confusion$n), cal$n_items)
  expect_setequal(names(confusion), names(as.data.frame(unclass(cal$confusion))))
  expect_identical(
    parsed$calibration$per_rater_agreement$rater,
    cal$per_rater_agreement$rater
  )
  expect_true("conf_low_exact" %in% names(parsed$calibration$agreement))
})

# Review follow-ups -------------------------------------------------------------

test_that("the default coefficient must have been computed", {
  gold <- tibble::tibble(
    human_label = rep(c("a", "b"), 15),
    judge_label = rep(c("a", "b"), 15)
  )
  kappa_only <- calibrate_judge(
    gold, coefficients = "cohen_kappa", n_boot = 50, seed = 1
  )
  expect_error(report(kappa_only, policy = edge_policy()), "gwet_ac1.*did not compute")
})

test_that("a policy coefficient must be a single name", {
  expect_error(
    edge_policy(coefficient = c("gwet_ac1", "gwet_ac2", "cohen_kappa", "krippendorff_alpha")),
    class = "yourhonor_policy_error"
  )
})

test_that("human reliability and agreement resolve the same Gwet coefficient", {
  base <- rep(c("a", "b"), 20)
  cal <- calibrate_judge(
    tibble::tibble(h1 = base, h2 = base, judge_label = base),
    human_cols = c("h1", "h2"), n_boot = 50, seed = 1
  )
  cal$weights <- NULL
  checks <- report(
    cal, policy = edge_policy(min_human_reliability_lower = 0.5)
  )$checks
  expect_true("agreement_gwet_ac1" %in% checks$check)
  expect_false(identical(
    checks$status[checks$check == "human_reliability"], "NOT_ASSESSED"
  ))
})

test_that("a reversed bootstrap interval is not masked by the exact bound", {
  cal <- calibrate_judge(
    tibble::tibble(
      human_label = rep(c("a", "b"), 30),
      judge_label = replace(rep(c("a", "b"), 30), 1, "b")
    ),
    coefficients = "gwet_ac1", n_boot = 50, seed = 1
  )
  cal$agreement$conf_low <- 0.95
  cal$agreement$conf_high <- 0.9
  cal$agreement$conf_low_exact <- 0.85
  checks <- report(cal, policy = edge_policy())$checks
  row <- checks[checks$check == "agreement_gwet_ac1", ]
  expect_identical(row$status, "INDETERMINATE")
  expect_match(row$reason, "reversed")
})

test_that("bootstrap resamples with a one-class reference are kept", {
  gold <- tibble::tibble(
    human_label = c(rep("a", 24), "b"),
    judge_label = c(rep("a", 23), "b", "b")
  )
  cal <- calibrate_judge(
    gold, coefficients = "gwet_ac1", n_boot = 400, seed = 1
  )
  # About a third of resamples, (24/25)^25, drop the single reference "b";
  # they still define AC1 and stay in. Only the (23/25)^25, about 12%, that
  # leave both raters on one category are discarded.
  expect_gt(cal$agreement$n_boot_effective, 0.8 * 400)
})

test_that("stability and agreement count the same chance categories", {
  runs <- data.frame(
    r1 = c(rep("a", 9), rep("b", 9), "a", "b"),
    r2 = c(rep("a", 9), rep("b", 9), "b", "a")
  )
  base <- summarize_stability_runs(runs, n_boot = 1)$stability
  extended <- summarize_stability_runs(
    rbind(runs, data.frame(r1 = "c", r2 = NA)), n_boot = 1
  )$stability
  expect_equal(extended, base)
  expect_equal(base, agreement(runs, coefficients = "gwet_ac1", n_boot = 10, seed = 1)$estimate)
})

test_that("a weighted perfect sample is assessed no worse than one error", {
  levels <- c("1", "2", "3")
  human <- rep(levels, 10)
  calibrate <- function(judge) {
    calibrate_judge(
      tibble::tibble(human_label = human, judge_label = judge),
      label_levels = levels, scale = "ordinal", weights = "quadratic",
      n_boot = 500, seed = 1
    )
  }
  perfect <- calibrate(human)
  one_error <- calibrate(replace(human, 1, "2"))
  policy <- edge_policy(min_agreement_lower = 0.9)
  assessed <- function(cal) {
    checks <- report(cal, policy = policy)$checks
    checks[checks$check == "agreement_gwet_ac2", ]
  }
  expect_true(is.finite(one_error$agreement$conf_low_exact[[1]]))
  expect_gte(assessed(perfect)$value, assessed(one_error)$value)
  expect_identical(assessed(perfect)$status, assessed(one_error)$status)
})
