# Evidence binding: audit ids, omitted or malformed evidence, and how the
# decision combines checks across a bundle.

test_that("a complete strong evidence bundle can pass a declared policy", {
  cal <- make_cal()
  bias <- make_bias()
  stability <- make_stability()
  policy <- make_policy(
    required_probes = "verbosity", max_pairwise_disagreement = 0.10
  )

  rep <- report(
    cal, bias = bias, stability = stability, policy = policy, output = "md"
  )
  expect_identical(rep$decision, "PASS")
  expect_all(rep$checks$status == "PASS")
  expect_setequal(
    rep$checks$check,
    c(
      "usable_items", "calibration_confidence_level",
      "calibration_missing_rate", "agreement_gwet_ac1", "class_support",
      "recall_I", "recall_P", "recall_C", "bias_verbosity_missing_rate",
      "bias_verbosity_n", "bias_verbosity_direction", "bias_verbosity",
      "stability_pairwise_disagreement", "stability_items",
      "stability_runs", "stability_missing_rate"
    )
  )
  expect_false(anyNA(rep$checks$op))
  expect_false(anyNA(rep$checks$threshold))
})

test_that("required but omitted evidence is never treated as a pass", {
  policy <- make_policy(
    required_probes = "verbosity", max_pairwise_disagreement = 0.05
  )
  rep <- report(make_cal(), policy = policy, output = "md")
  expect_identical(rep$decision, "NOT_ASSESSED")
  expect_identical(check_status(rep, "bias_verbosity"), "NOT_ASSESSED")
  expect_identical(
    check_status(rep, "stability_pairwise_disagreement"), "NOT_ASSESSED"
  )
  # Prerequisite rows keep the gate they stand in for.
  row <- rep$checks[rep$checks$check == "bias_verbosity", ]
  expect_identical(row$op, "upper <=")
  expect_identical(row$threshold, 0.1)
  expect_true(is.na(row$value))
})

test_that("unbound secondary evidence cannot satisfy a policy", {
  bias <- make_bias(audit_id = NULL)
  rep <- report(
    make_cal(), bias = bias,
    policy = make_policy(required_probes = "verbosity"), output = "md"
  )
  expect_identical(rep$decision, "NOT_ASSESSED")
  expect_match(paste(format(rep), collapse = "\n"), "UNBOUND")
})

test_that("two missing or empty audit ids do not bind evidence", {
  cal <- make_cal()
  cal$audit_id <- NULL
  bias <- make_bias(audit_id = NULL)
  expect_false(evidence_is_bound(cal, bias))
  cal$audit_id <- NA_character_
  attr(bias, "audit_id") <- NA_character_
  expect_false(evidence_is_bound(cal, bias))
  cal$audit_id <- ""
  attr(bias, "audit_id") <- ""
  expect_false(evidence_is_bound(cal, bias))
  cal$audit_id <- "bundle"
  attr(bias, "audit_id") <- "bundle"
  expect_true(evidence_is_bound(cal, bias))
  expect_identical(audit_id(cal), "bundle")
  expect_identical(audit_id(bias), "bundle")
  expect_identical(audit_id(make_stability()), "test-bundle")
})

test_that("mismatched evidence bundle IDs error instead of being combined", {
  bias <- make_bias(audit_id = "different-bundle")
  expect_error(
    report(make_cal(), bias = bias, output = "md"),
    class = "yourhonor_binding_error"
  )
})

test_that("a failure dominates missing secondary evidence", {
  gold <- tibble::tibble(
    human_label = rep(c("I", "P", "C"), each = 20),
    judge_label = c(rep("C", 20), rep("P", 20), rep("C", 20))
  )
  cal <- calibrate_judge(
    gold, label_levels = c("I", "P", "C"), coefficients = "gwet_ac1",
    n_boot = 50, seed = 1
  )
  rep <- report(
    cal,
    policy = make_policy(required_probes = "verbosity"),
    output = "md"
  )
  expect_identical(rep$decision, "FAIL")
  expect_true(any(rep$checks$status == "NOT_ASSESSED"))
})

test_that("malformed evidence tables are indeterminate or not assessed", {
  cal <- make_cal()
  doubled <- cal
  doubled$agreement <- vctrs::vec_rbind(cal$agreement, cal$agreement)
  rep <- report(doubled, policy = make_policy(), output = "md")
  row <- rep$checks[rep$checks$check == "agreement_gwet_ac1", ]
  expect_identical(row$status, "INDETERMINATE")
  expect_match(row$reason, "Multiple rows")

  empty <- cal
  empty$confusion_metrics <- cal$confusion_metrics[0, ]
  rep <- report(empty, policy = make_policy(), output = "md")
  expect_identical(check_status(rep, "class_support"), "NOT_ASSESSED")
  expect_identical(rep$decision, "NOT_ASSESSED")
  expect_none(startsWith(rep$checks$check, "recall_"))

  reversed <- interval_min_check("x", 0.9, 0.5, 0.8)
  expect_identical(reversed$status, "INDETERMINATE")
  expect_match(reversed$reason, "reversed")
  expect_true(is.na(reversed$value))
  expect_match(
    interval_max_check("x", 0.9, 0.5, 0.8)$reason, "reversed"
  )
})
