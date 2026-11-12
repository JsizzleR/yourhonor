# Calibration and stability gates: agreement, class recall and precision,
# human reliability, stability evidence and confidence levels.

test_that("rare-class failure cannot be hidden by high aggregate AC1", {
  gold <- tibble::tibble(
    human_label = c(rep("C", 990), rep("I", 10)),
    judge_label = rep("C", 1000)
  )
  cal <- calibrate_judge(
    gold, label_levels = c("I", "C"), coefficients = "gwet_ac1",
    n_boot = 100, seed = 1
  )
  policy <- acceptance_policy(
    min_agreement_lower = 0.9,
    min_n = 100,
    min_class_n = 5,
    min_recall = c(I = 0.8),
    max_missing_rate = 0,
    max_pairwise_disagreement = NULL,
    coefficient = "gwet_ac1"
  )
  rep <- report(cal, policy = policy, output = "md")

  expect_gt(cal$agreement$conf_low, 0.9)
  expect_equal(cal$confusion_metrics$recall[cal$confusion_metrics$label == "I"], 0)
  expect_identical(rep$decision, "FAIL")
  expect_identical(check_status(rep, "recall_I"), "FAIL")
})

test_that("a policy coefficient the calibration lacks is an error", {
  policy <- make_policy()
  policy$coefficient <- "cohen_kappa"
  expect_error(
    report(make_cal(), policy = policy, output = "md"),
    "cohen_kappa.*did not compute", class = "yourhonor_policy_error"
  )
})

test_that("stability evidence at another confidence level is not assessed", {
  stability <- test_retest(
    tibble::tibble(input = paste("answer", 1:60)), flat_judge,
    input_cols = "input", n_runs = 3, label_levels = c("I", "P", "C"),
    conf_level = 0.9, n_boot = 50, seed = 1, audit_id = "test-bundle"
  )
  rep <- report(
    make_cal(), stability = stability,
    policy = make_policy(max_pairwise_disagreement = 0.1), output = "md"
  )
  expect_identical(rep$decision, "INDETERMINATE")
  expect_identical(
    check_status(rep, "stability_confidence_level"), "INDETERMINATE"
  )
  expect_identical(check_status(rep, "stability_items"), "PASS")
  expect_identical(check_status(rep, "stability_runs"), "PASS")
})

test_that("stability missingness is a policy failure", {
  calls <- 0L
  repeated_judge <- function(d) {
    calls <<- calls + 1L
    if (calls == 3L) rep(NA_character_, nrow(d)) else rep(c("I", "C"), 30)
  }
  stability <- test_retest(
    tibble::tibble(input = paste("answer", 1:60)), repeated_judge,
    input_cols = "input", n_runs = 3,
    label_levels = c("I", "P", "C"), n_boot = 50, seed = 1,
    audit_id = "test-bundle"
  )
  policy <- acceptance_policy(
    min_agreement_lower = 0.9, min_n = 50, min_class_n = 15,
    min_recall = 0.8, max_missing_rate = 0.1,
    max_pairwise_disagreement = 0.05,
    min_stability_n = 50, min_stability_runs = 3,
    max_stability_missing_rate = 0.1,
    coefficient = "gwet_ac1"
  )
  rep <- report(make_cal(), stability = stability, policy = policy, output = "md")
  expect_identical(rep$decision, "FAIL")
  expect_identical(check_status(rep, "stability_missing_rate"), "FAIL")
  row <- rep$checks[rep$checks$check == "stability_missing_rate", ]
  expect_identical(row$op, "<=")
  expect_identical(row$threshold, 0.1)
  expect_equal(row$value, 1 / 3)
})

test_that("tiny stability samples cannot satisfy a policy", {
  stability <- test_retest(
    tibble::tibble(input = "only item"),
    function(d) "C", input_cols = "input", n_runs = 2, n_boot = 50, seed = 1,
    audit_id = "test-bundle"
  )
  policy <- acceptance_policy(
    min_agreement_lower = 0.9, min_n = 50, min_class_n = 15,
    min_recall = 0.8, max_missing_rate = 0,
    max_pairwise_disagreement = 0.10,
    min_stability_n = 50, min_stability_runs = 3,
    max_stability_missing_rate = 0,
    coefficient = "gwet_ac1"
  )
  rep <- report(make_cal(), stability = stability, policy = policy, output = "md")
  expect_identical(rep$decision, "INDETERMINATE")
  expect_identical(check_status(rep, "stability_items"), "INDETERMINATE")
  expect_identical(check_status(rep, "stability_runs"), "INDETERMINATE")
})

test_that("single-class calibration stays indeterminate", {
  gold <- tibble::tibble(
    human_label = rep("C", 60),
    judge_label = rep("C", 60)
  )
  cal <- calibrate_judge(
    gold, label_levels = c("I", "C"), coefficients = "gwet_ac1",
    n_boot = 50, seed = 1
  )
  rep <- report(cal, policy = make_policy(), output = "md")
  expect_identical(rep$decision, "INDETERMINATE")
  expect_identical(check_status(rep, "class_support"), "INDETERMINATE")
  expect_identical(check_status(rep, "recall_I"), "INDETERMINATE")
  expect_match(
    rep$checks$reason[rep$checks$check == "recall_I"],
    "Class interval is undefined"
  )
})

test_that("a zero-width perfect-sample bootstrap is assessed on the exact bound", {
  perfect <- function(each) {
    labels <- rep(c("I", "P", "C"), each = each)
    calibrate_judge(
      tibble::tibble(human_label = labels, judge_label = labels),
      label_levels = c("I", "P", "C"), coefficients = "gwet_ac1",
      n_boot = 50, seed = 1
    )
  }
  large <- perfect(100)
  rep <- report(large, policy = make_policy(), output = "md")
  expect_true(large$agreement$boundary_degenerate)
  expect_identical(check_status(rep, "agreement_gwet_ac1"), "PASS")
  row <- rep$checks[rep$checks$check == "agreement_gwet_ac1", ]
  expect_equal(row$value, large$agreement$conf_low_exact)
  expect_lt(row$value, 1)

  # Fifteen perfect items cannot support a 0.9 lower bound.
  small <- perfect(5)
  rep <- report(small, policy = make_policy(), output = "md")
  expect_true(small$agreement$boundary_degenerate)
  expect_identical(check_status(rep, "agreement_gwet_ac1"), "INDETERMINATE")
})

test_that("policy confidence level must match the evidence", {
  rep <- report(
    make_cal(conf_level = 0.90), policy = make_policy(), output = "md"
  )
  expect_identical(rep$decision, "INDETERMINATE")
  expect_identical(
    check_status(rep, "calibration_confidence_level"), "INDETERMINATE"
  )
  row <- rep$checks[rep$checks$check == "calibration_confidence_level", ]
  expect_identical(row$op, "=")
  expect_identical(row$value, 0.9)
  expect_identical(row$threshold, 0.95)
})

test_that("a policy can require reliable multi-human gold labels", {
  base <- rep(c("I", "P", "C"), each = 100)
  shifted <- unname(c(I = "P", P = "C", C = "I")[base])
  cal <- calibrate_judge(
    tibble::tibble(h1 = base, h2 = shifted, h3 = base, judge_label = base),
    human_cols = c("h1", "h2", "h3"), label_levels = c("I", "P", "C"),
    coefficients = "gwet_ac1", n_boot = 100, seed = 1,
    audit_id = "test-bundle"
  )
  policy <- make_policy(min_human_reliability_lower = 0.9)
  rep <- report(cal, policy = policy, output = "md")
  expect_true("human_reliability" %in% rep$checks$check)
  expect_identical(check_status(rep, "human_reliability"), "FAIL")
  expect_identical(rep$decision, "FAIL")

  single_policy <- make_policy(min_human_reliability_lower = 0.5)
  single <- report(make_cal(), policy = single_policy, output = "md")
  expect_identical(single$decision, "NOT_ASSESSED")
  expect_identical(check_status(single, "human_reliability"), "NOT_ASSESSED")
})

test_that("human reliability can pass or be indeterminate", {
  base <- rep(c("I", "P", "C"), each = 100)
  agreeing <- base
  agreeing[c(1, 101, 201)] <- c("P", "C", "I")
  cal <- calibrate_judge(
    tibble::tibble(h1 = base, h2 = agreeing, h3 = base, judge_label = base),
    human_cols = c("h1", "h2", "h3"), label_levels = c("I", "P", "C"),
    coefficients = "gwet_ac1", n_boot = 100, seed = 1,
    audit_id = "test-bundle"
  )
  passing <- report(
    cal, policy = make_policy(min_human_reliability_lower = 0.9),
    output = "md"
  )
  expect_identical(check_status(passing, "human_reliability"), "PASS")
  row <- passing$checks[passing$checks$check == "human_reliability", ]
  expect_identical(row$op, "lower >=")
  expect_identical(row$threshold, 0.9)
  expect_gte(row$value, 0.9)

  # A small perfect three-rater sample pins the bootstrap at 1; its exact
  # bound is too wide to clear the threshold.
  small <- rep(c("I", "P", "C"), each = 5)
  perfect <- calibrate_judge(
    tibble::tibble(h1 = small, h2 = small, h3 = small, judge_label = small),
    human_cols = c("h1", "h2", "h3"), label_levels = c("I", "P", "C"),
    coefficients = "gwet_ac1", n_boot = 100, seed = 1,
    audit_id = "test-bundle"
  )
  hr <- perfect$human_reliability
  expect_false(hr$ci_reliable[hr$coefficient == "gwet_ac1"])
  indeterminate <- report(
    perfect, policy = make_policy(min_human_reliability_lower = 0.9),
    output = "md"
  )
  expect_identical(
    check_status(indeterminate, "human_reliability"), "INDETERMINATE"
  )
})

test_that("min_precision gates per-class precision end to end", {
  gold <- tibble::tibble(
    human_label = rep(c("I", "P", "C"), each = 100),
    judge_label = c(rep("I", 100), rep("P", 100), rep("P", 50), rep("C", 50))
  )
  cal <- calibrate_judge(
    gold, label_levels = c("I", "P", "C"), coefficients = "gwet_ac1",
    n_boot = 100, seed = 1, audit_id = "test-bundle"
  )
  rep <- report(
    cal, policy = make_policy(min_precision = c(P = 0.9)), output = "md"
  )
  expect_identical(check_status(rep, "precision_P"), "FAIL")
  expect_false("precision_I" %in% rep$checks$check)
  row <- rep$checks[rep$checks$check == "precision_P", ]
  expect_identical(row$op, "lower >=")
  expect_identical(row$threshold, 0.9)
  expect_identical(
    row$value,
    cal$confusion_metrics$precision_conf_low[cal$confusion_metrics$label == "P"]
  )
  global <- report(cal, policy = make_policy(min_precision = 0.5), output = "md")
  expect_all(c("precision_I", "precision_P", "precision_C") %in%
    global$checks$check)
})

test_that("a policy naming an unknown class is NOT_ASSESSED, not ignored", {
  rep <- report(
    make_cal(), policy = make_policy(min_precision = c(X = 0.5)), output = "md"
  )
  expect_identical(rep$decision, "NOT_ASSESSED")
  row <- rep$checks[rep$checks$check == "precision_X", ]
  expect_identical(row$status, "NOT_ASSESSED")
  expect_match(row$reason, "absent from the calibration label universe")
})

test_that("an untrusted interval reports NA and its cause, not the estimate", {
  labels <- rep(c("I", "P", "C"), each = 100)
  cal <- calibrate_judge(
    tibble::tibble(human_label = labels, judge_label = labels),
    label_levels = c("I", "P", "C"), coefficients = "gwet_ac1",
    n_boot = 50, seed = 1
  )
  # A degenerate row without an exact bound, as in a calibration made before
  # `conf_low_exact` existed, has no usable lower bound at all.
  cal$agreement$conf_low_exact <- NULL
  rep <- report(cal, policy = make_policy(), output = "md")
  row <- rep$checks[rep$checks$check == "agreement_gwet_ac1", ]
  expect_identical(row$status, "INDETERMINATE")
  expect_true(is.na(row$value))
  expect_match(row$reason, "degenerate at the boundary")
  expect_false(grepl("not reliable: ok", row$reason, fixed = TRUE))
  expect_match(
    paste(format(rep), collapse = "\n"),
    "`agreement_gwet_ac1` = NA \\(lower >= 0.9\\)"
  )
})

test_that("a calibration at another confidence level withholds intervals", {
  base <- rep(c("I", "P", "C"), each = 100)
  cal <- calibrate_judge(
    tibble::tibble(h1 = base, h2 = base, h3 = base, judge_label = base),
    human_cols = c("h1", "h2", "h3"), label_levels = c("I", "P", "C"),
    coefficients = "gwet_ac1", n_boot = 100, seed = 1, conf_level = 0.9,
    audit_id = "test-bundle"
  )
  rep <- report(
    cal,
    policy = make_policy(
      min_human_reliability_lower = 0.9, min_precision = 0.5
    ),
    output = "md"
  )
  expect_identical(rep$decision, "INDETERMINATE")
  interval_checks <- c(
    "agreement_gwet_ac1", "human_reliability", "recall_I", "recall_P",
    "recall_C", "precision_I", "precision_P", "precision_C"
  )
  rows <- rep$checks[rep$checks$check %in% interval_checks, ]
  expect_identical(nrow(rows), length(interval_checks))
  expect_all(rows$status == "INDETERMINATE")
  expect_all(is.na(rows$value))
  expect_all(grepl("confidence level does not match", rows$reason))
  expect_identical(check_status(rep, "usable_items"), "PASS")
  expect_identical(check_status(rep, "calibration_missing_rate"), "PASS")
  expect_identical(check_status(rep, "class_support"), "PASS")
  expect_none(rep$checks$status == "FAIL")
})

# Policy coefficient follow-ups --------------------------------------------------

test_that("a default coefficient the calibration lacks is NOT_ASSESSED", {
  gold <- tibble::tibble(
    human_label = rep(c("a", "b"), 15),
    judge_label = rep(c("a", "b"), 15)
  )
  kappa_only <- calibrate_judge(
    gold, coefficients = "cohen_kappa", n_boot = 50, seed = 1
  )
  policy <- acceptance_policy(
    min_agreement_lower = 0.5, min_n = 20, min_class_n = 1, min_recall = 0,
    max_missing_rate = 0.5, max_pairwise_disagreement = NULL
  )
  rep <- report(kappa_only, policy = policy, output = "md")
  row <- rep$checks[rep$checks$check == "agreement_gwet_ac1", ]
  expect_identical(row$status, "NOT_ASSESSED")
  expect_match(row$reason, "did not compute gwet_ac1")
  expect_identical(rep$decision, "NOT_ASSESSED")
  # The other checks are still assessed on their own evidence.
  expect_identical(check_status(rep, "usable_items"), "PASS")
  expect_identical(check_status(rep, "calibration_missing_rate"), "PASS")
  expect_identical(check_status(rep, "class_support"), "PASS")
  expect_match(
    paste(format(rep), collapse = "\n"), "gwet_ac1\\): not estimable"
  )

  path <- withr::local_tempfile(fileext = ".json")
  expect_no_error(write_audit(kappa_only, path, policy = policy))
  audit <- jsonlite::fromJSON(path)
  expect_identical(audit$decision, "NOT_ASSESSED")
  expect_identical(
    audit$checks$status[audit$checks$check == "agreement_gwet_ac1"],
    "NOT_ASSESSED"
  )
  # Naming the uncomputed coefficient is still a configuration error.
  expect_error(
    report(kappa_only, policy = make_policy()), "gwet_ac1.*did not compute",
    class = "yourhonor_policy_error"
  )
})

test_that("a degenerate row's exact bound is capped by the bootstrap bound", {
  cal <- make_cal()
  i <- cal$agreement$coefficient == "gwet_ac1"
  # A zero-width bootstrap below an exact bound that clears the threshold:
  # the exact bound alone would PASS, the smaller of the two cannot.
  cal$agreement$boundary_degenerate[i] <- TRUE
  cal$agreement$ci_reliable[i] <- FALSE
  cal$agreement$conf_low[i] <- 0.85
  cal$agreement$conf_high[i] <- 0.85
  cal$agreement$conf_low_exact[i] <- 0.95
  row <- assess_calibration(cal, NULL, NULL, make_policy())$checks
  row <- row[row$check == "agreement_gwet_ac1", ]
  expect_identical(row$status, "INDETERMINATE")
  expect_identical(row$value, 0.85)

  # Without an exact bound the degenerate bootstrap is not trusted at all.
  cal$agreement$conf_low_exact[i] <- NA_real_
  row <- assess_calibration(cal, NULL, NULL, make_policy())$checks
  row <- row[row$check == "agreement_gwet_ac1", ]
  expect_identical(row$status, "INDETERMINATE")
  expect_match(row$reason, "degenerate at the boundary")
})
