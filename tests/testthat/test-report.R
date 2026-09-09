make_cal <- function(n_boot = 100, conf_level = 0.95) {
  human <- rep(c("I", "P", "C"), each = 100)
  judge <- human
  judge[c(1, 101, 201)] <- c("P", "C", "P")
  gold <- tibble::tibble(
    human_label = human,
    judge_label = judge
  )
  calibrate_judge(
    gold, label_levels = c("I", "P", "C"),
    coefficients = "gwet_ac1", conf_level = conf_level,
    n_boot = n_boot, seed = 1,
    audit_id = "test-bundle"
  )
}

# Dependent thresholds travel with their gate: they are supplied only when
# the gate is on, exactly as acceptance_policy() requires.
make_policy <- function(required_probes = character(0),
                        max_pairwise_disagreement = NULL,
                        min_human_reliability_lower = NULL,
                        min_precision = NULL,
                        min_probe_n = 20,
                        conf_level = 0.95) {
  args <- list(
    min_agreement_lower = 0.9,
    min_n = 50,
    min_class_n = 15,
    min_recall = 0.8,
    max_missing_rate = 0,
    max_pairwise_disagreement = max_pairwise_disagreement,
    required_probes = required_probes,
    min_human_reliability_lower = min_human_reliability_lower,
    min_precision = min_precision,
    conf_level = conf_level,
    coefficient = "gwet_ac1"
  )
  if (!is.null(max_pairwise_disagreement)) {
    args <- c(args, list(
      min_stability_n = 50, min_stability_runs = 3,
      max_stability_missing_rate = 0
    ))
  }
  if (length(required_probes) > 0L) {
    args <- c(args, list(
      min_probe_n = min_probe_n, max_probe_flip_rate = 0.1,
      max_probe_directional_rate = 0.1, max_probe_missing_rate = 0
    ))
  }
  do.call(acceptance_policy, args)
}

flat_judge <- function(d) rep("C", nrow(d))

length_judge <- function(d) ifelse(nchar(d$input) > 40, "C", "P")

make_bias <- function(judge = flat_judge, probes = "verbosity", n = 60,
                      audit_id = "test-bundle") {
  probe_bias(
    tibble::tibble(input = paste("answer", seq_len(n))), judge,
    probes = probes, seed = 1, audit_id = audit_id
  )
}

make_stability <- function(judge = flat_judge, n = 60,
                           audit_id = "test-bundle") {
  test_retest(
    tibble::tibble(input = paste("answer", seq_len(n))), judge,
    input_cols = "input", n_runs = 3, label_levels = c("I", "P", "C"),
    n_boot = 50, seed = 1, audit_id = audit_id
  )
}

check_status <- function(rep, check) {
  rep$checks$status[rep$checks$check == check]
}

test_that("reports are descriptive until a policy is supplied", {
  rep <- report(make_cal(), output = "md")
  expect_s3_class(rep, "yourhonor_report")
  expect_identical(rep$decision, "NOT_ASSESSED")
  expect_identical(rep$checks$status, "NOT_ASSESSED")
  expect_true(is.na(rep$checks$op))
  expect_true(is.na(rep$checks$threshold))
  txt <- paste(format(rep), collapse = "\n")
  expect_match(txt, "Agreement with humans")
  expect_match(txt, "Assessment: \\*\\*NOT_ASSESSED\\*\\*")
  expect_match(txt, "Bias probes: \\*\\*NOT ASSESSED\\*\\*")
  expect_match(txt, "Test-retest stability: \\*\\*NOT ASSESSED\\*\\*")
})

test_that("report() returns one visible object type for every output", {
  cal <- make_cal()
  console <- report(cal)
  md <- report(cal, output = "md")
  expect_s3_class(console, "yourhonor_report")
  expect_identical(console$output, "console")
  expect_identical(md$output, "md")
  expect_true(withVisible(report(cal))$visible)
  expect_true(withVisible(report(cal, output = "md"))$visible)
  expect_identical(console$decision, md$decision)
  expect_identical(console$checks, md$checks)
  expect_identical(
    names(console),
    c("decision", "checks", "calibration", "bias", "stability", "policy",
      "output")
  )
  # The rendering is chosen at print time, not fixed at construction.
  expect_identical(format(console, output = "md"), format(md))
  expect_identical(as.character(console), format(md, output = "md"))
  expect_match(
    paste(format(md, output = "console"), collapse = "\n"),
    "yourhonor assessment: NOT_ASSESSED"
  )
})

test_that("markdown reports print as plain lines", {
  rep <- report(make_cal(), output = "md")
  out <- capture.output(print(rep))
  expect_match(paste(out, collapse = "\n"), "Judge evidence")
  expect_identical(out, format(rep))
  console_out <- cli::cli_fmt(print(report(make_cal()), output = "console"))
  expect_match(paste(console_out, collapse = "\n"), "yourhonor assessment")
})

test_that("console reports the explicit assessment state", {
  out <- cli::cli_fmt(print(report(make_cal(), output = "console")))
  expect_match(paste(out, collapse = "\n"), "yourhonor assessment: NOT_ASSESSED")
  expect_match(paste(out, collapse = "\n"), "Descriptive only")
})

test_that("console reports render policy, bias, and stability evidence", {
  cal <- make_cal()
  bias <- make_bias()
  stability <- make_stability()
  policy <- make_policy(
    required_probes = "verbosity", max_pairwise_disagreement = 0.10
  )
  rep <- report(cal, bias = bias, stability = stability, policy = policy)
  out <- paste(cli::cli_fmt(print(rep)), collapse = "\n")
  expect_match(out, "yourhonor assessment: PASS")
  expect_match(out, "Required bias probes cleared the declared policy gates")
  expect_match(out, "Pairwise disagreement: 0.00")
  expect_match(out, "policy checks")
  expect_match(out, "PASS: usable_items = 300 \\(>= 50\\)")
  expect_match(out, "PASS: bias_verbosity = 0\\.\\d+ \\(upper <= 0.1\\)")
  expect_false(grepl("UNBOUND", out))
})

test_that("check lines show the value and threshold behind a failure", {
  gold <- tibble::tibble(
    human_label = c(rep("C", 990), rep("I", 10)),
    judge_label = rep("C", 1000)
  )
  cal <- calibrate_judge(
    gold, label_levels = c("I", "C"), coefficients = "gwet_ac1",
    n_boot = 100, seed = 1
  )
  policy <- acceptance_policy(
    min_agreement_lower = 0.9, min_n = 100, min_class_n = 5,
    min_recall = c(I = 0.8), max_missing_rate = 0,
    max_pairwise_disagreement = NULL, coefficient = "gwet_ac1"
  )
  rep <- report(cal, policy = policy, output = "md")
  row <- rep$checks[rep$checks$check == "recall_I", ]
  expect_identical(row$op, "lower >=")
  expect_identical(row$threshold, 0.8)
  expect_identical(row$value, 0)
  md <- paste(format(rep), collapse = "\n")
  expect_match(md, "- \\*\\*FAIL\\*\\* `recall_I` = 0 \\(lower >= 0.8\\): ")
  console <- paste(format(rep, output = "console"), collapse = "\n")
  expect_match(console, "FAIL: recall_I = 0 \\(lower >= 0.8\\) - ")
})

test_that("supplied probes outside a policy are not described as clearing it", {
  bias <- make_bias(length_judge, n = 30)
  rep <- report(make_cal(), bias = bias, policy = make_policy(), output = "md")
  txt <- paste(format(rep), collapse = "\n")
  expect_match(txt, "outside the policy; descriptive probe settings flagged verbosity")
  expect_false(grepl("cleared the declared policy", txt))
  # The same branch without a detected signal.
  quiet <- report(
    make_cal(), bias = make_bias(), policy = make_policy(), output = "md"
  )
  expect_match(
    paste(format(quiet), collapse = "\n"),
    "outside the policy; descriptive probe settings did not flag"
  )
})

test_that("a non-required probe that detected bias is named next to the gate", {
  cal <- make_cal()
  clean <- make_bias(flat_judge, probes = "verbosity")
  biased <- make_bias(length_judge, probes = "verbosity")
  # `bandwagon` is not gated. A judge that follows the crowd flags it; the
  # length-sensitive judge would too, since the prefix changes length, but
  # this one isolates the effect.
  crowd_judge <- function(d) ifelse(grepl("looks right", d$input), "C", "P")
  bandwagon <- make_bias(crowd_judge, probes = "bandwagon")
  combined <- combine_bias(clean, bandwagon)
  expect_identical(combined$verdict[combined$probe == "bandwagon"], "BIAS")

  rep <- report(
    cal, bias = combined, policy = make_policy(required_probes = "verbosity"),
    output = "md"
  )
  txt <- paste(format(rep), collapse = "\n")
  expect_identical(rep$decision, "PASS")
  expect_match(txt, "Required bias probes cleared the declared policy gates")
  expect_match(txt, "Outside the policy, descriptive probe settings flagged bandwagon")

  failing <- report(
    cal, bias = combine_bias(biased, bandwagon),
    policy = make_policy(required_probes = "verbosity"), output = "md"
  )
  failing_txt <- paste(format(failing), collapse = "\n")
  expect_match(failing_txt, "Policy-failing bias checks: bias_verbosity")
  expect_match(failing_txt, "flagged bandwagon")
})

test_that("bias summaries without a policy report every verdict state", {
  cal <- make_cal()
  detected <- report(cal, bias = make_bias(length_judge), output = "md")
  expect_match(
    paste(format(detected), collapse = "\n"),
    "Descriptive probe settings detected bias: verbosity"
  )
  silent <- report(cal, bias = make_bias(), output = "md")
  expect_match(
    paste(format(silent), collapse = "\n"),
    "found no directional signal; absence is not established"
  )
  abstaining <- make_bias(function(d) rep(NA_character_, nrow(d)))
  expect_identical(abstaining$verdict, "INCONCLUSIVE")
  inconclusive <- report(cal, bias = abstaining, output = "md")
  expect_match(
    paste(format(inconclusive), collapse = "\n"),
    "At least one supplied bias probe was INCONCLUSIVE"
  )
})

test_that("incomplete required-probe evidence is summarised as such", {
  bias <- make_bias(n = 1)
  rep <- report(
    make_cal(), bias = bias,
    policy = make_policy(required_probes = "verbosity"), output = "md"
  )
  expect_identical(rep$decision, "INDETERMINATE")
  expect_match(
    paste(format(rep), collapse = "\n"),
    "Bias policy evidence is incomplete or indeterminate"
  )
})

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
  expect_true(all(rep$checks$status == "PASS"))
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

test_that("a missing policy coefficient is named instead of silently replaced", {
  policy <- make_policy()
  policy$coefficient <- "cohen_kappa"
  rep <- report(make_cal(), policy = policy, output = "md")
  text <- paste(format(rep), collapse = "\n")
  expect_match(text, "cohen_kappa.*not estimable")
  expect_identical(rep$decision, "NOT_ASSESSED")
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
  expect_error(report(make_cal(), bias = bias, output = "md"), "do not match")
})

test_that("small bias probes stay indeterminate despite no detected signal", {
  bias <- make_bias(n = 1)
  rep <- report(
    make_cal(), bias = bias,
    policy = make_policy(required_probes = "verbosity"), output = "md"
  )
  expect_identical(rep$decision, "INDETERMINATE")
  expect_identical(check_status(rep, "bias_verbosity"), "INDETERMINATE")
  expect_identical(check_status(rep, "bias_verbosity_n"), "INDETERMINATE")
})

test_that("a probe with no usable evidence is indeterminate", {
  bias <- make_bias(function(d) rep(NA_character_, nrow(d)))
  rep <- report(
    make_cal(), bias = bias,
    policy = make_policy(required_probes = "verbosity"), output = "md"
  )
  expect_identical(check_status(rep, "bias_verbosity_missing_rate"), "FAIL")
  row <- rep$checks[rep$checks$check == "bias_verbosity", ]
  expect_identical(row$status, "INDETERMINATE")
  expect_match(row$reason, "did not yield usable evidence: insufficient_data")
  expect_false("bias_verbosity_n" %in% rep$checks$check)
})

test_that("a multi-row required probe is indeterminate", {
  bias <- make_bias()
  doubled <- vctrs::vec_rbind(bias, bias)
  attr(doubled, "audit_id") <- "test-bundle"
  class(doubled) <- class(bias)
  rep <- report(
    make_cal(), bias = doubled,
    policy = make_policy(required_probes = "verbosity"), output = "md"
  )
  expect_identical(rep$decision, "INDETERMINATE")
  row <- rep$checks[rep$checks$check == "bias_verbosity", ]
  expect_identical(row$status, "INDETERMINATE")
  expect_match(row$reason, "Multiple rows")
})

test_that("bias evidence at another confidence level is not assessed", {
  bias <- probe_bias(
    tibble::tibble(input = paste("answer", 1:60)), flat_judge,
    probes = "verbosity", seed = 1, conf_level = 0.9,
    audit_id = "test-bundle"
  )
  rep <- report(
    make_cal(), bias = bias,
    policy = make_policy(required_probes = "verbosity"), output = "md"
  )
  expect_identical(rep$decision, "INDETERMINATE")
  expect_identical(
    check_status(rep, "bias_verbosity_confidence_level"), "INDETERMINATE"
  )
  expect_false("bias_verbosity" %in% rep$checks$check)
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

test_that("a zero-width perfect-sample bootstrap cannot clear policy", {
  labels <- rep(c("I", "P", "C"), each = 100)
  cal <- calibrate_judge(
    tibble::tibble(human_label = labels, judge_label = labels),
    label_levels = c("I", "P", "C"), coefficients = "gwet_ac1",
    n_boot = 50, seed = 1
  )
  rep <- report(cal, policy = make_policy(), output = "md")
  expect_true(cal$agreement$boundary_degenerate)
  expect_identical(rep$decision, "INDETERMINATE")
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

test_that("selective probe abstention fails the declared missingness bound", {
  selective <- function(d) {
    item <- as.integer(sub(".*answer ([0-9]+).*", "\\1", d$input))
    ifelse(item > 90L, "C", NA_character_)
  }
  bias <- probe_bias(
    tibble::tibble(input = paste("answer", 1:100)), selective,
    probes = "verbosity", seed = 1, audit_id = "test-bundle"
  )
  policy <- acceptance_policy(
    min_agreement_lower = 0.9, min_n = 50, min_class_n = 15,
    min_recall = 0.8, max_missing_rate = 0,
    max_pairwise_disagreement = NULL,
    required_probes = "verbosity", min_probe_n = 10,
    max_probe_flip_rate = 0.5, max_probe_directional_rate = 0.5,
    max_probe_missing_rate = 0.1,
    coefficient = "gwet_ac1"
  )
  rep <- report(make_cal(), bias = bias, policy = policy, output = "md")
  expect_equal(bias$n_scored, 10)
  expect_equal(bias$missing_rate, 0.9)
  expect_identical(rep$decision, "FAIL")
  expect_identical(check_status(rep, "bias_verbosity_missing_rate"), "FAIL")
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

  # A perfect three-rater sample pins the bootstrap at 1 and is degenerate.
  perfect <- calibrate_judge(
    tibble::tibble(h1 = base, h2 = base, h3 = base, judge_label = base),
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
  expect_true(all(c("precision_I", "precision_P", "precision_C") %in%
    global$checks$check))
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

test_that("a required position probe flows through report() and write_audit()", {
  pairs <- tibble::tibble(
    context = paste("question", 1:60),
    a = paste("answer a", 1:60),
    b = paste("answer b", 1:60)
  )
  # Content-consistent: picks candidate A wherever it is displayed.
  fair_judge <- function(d) {
    ifelse(grepl("answer a", d$.candidate_1, fixed = TRUE), "A", "B")
  }
  position <- probe_position(
    pairs, fair_judge, candidate_a_col = "a", candidate_b_col = "b",
    context_cols = "context", seed = 1, audit_id = "test-bundle"
  )
  combined <- combine_bias(make_bias(), position)
  policy <- make_policy(required_probes = c("verbosity", "position"))
  rep <- report(make_cal(), bias = combined, policy = policy, output = "md")
  expect_identical(rep$decision, "PASS")
  expect_true(all(c("bias_position", "bias_position_direction",
    "bias_position_n", "bias_position_missing_rate") %in% rep$checks$check))
  expect_match(
    paste(format(rep), collapse = "\n"),
    "Required bias probes cleared the declared policy gates"
  )

  skip_if_not_installed("jsonlite")
  path <- withr::local_tempfile(fileext = ".json")
  write_audit(
    make_cal(), path, bias = combined, policy = policy, format = "json"
  )
  parsed <- jsonlite::fromJSON(path)
  expect_identical(parsed$decision, "PASS")
  expect_setequal(parsed$bias$results$probe, c("verbosity", "position"))
  expect_identical(
    names(parsed$checks),
    c("check", "status", "value", "op", "threshold", "reason")
  )
  expect_identical(
    parsed$checks$op[parsed$checks$check == "bias_position"], "upper <="
  )
  expect_identical(
    parsed$checks$threshold[parsed$checks$check == "bias_position"], 0.1
  )
  expect_identical(parsed$policy$required_probes, c("verbosity", "position"))
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

test_that("policy controls validate", {
  expect_error(
    acceptance_policy(.8, 50, 10, .8, 0, NULL, required_probes = "unknown"),
    "Unknown `required_probes`"
  )
  expect_error(
    acceptance_policy(
      .8, 50, 10, .8, 0, NULL, required_probes = c("verbosity", "verbosity"),
      min_probe_n = 10, max_probe_flip_rate = .1,
      max_probe_directional_rate = .1, max_probe_missing_rate = 0
    ),
    "must not repeat"
  )
  expect_error(
    acceptance_policy(.8, 50, 10, .8, 0, NULL, required_probes = NA),
    "character vector of probe names"
  )
  expect_error(
    acceptance_policy(.8, 50, 10, c(I = 1.2), 0, NULL),
    "min_recall"
  )
  expect_error(
    acceptance_policy(.8, 50, 10, .8, 0, NULL, min_precision = c(1, 2)),
    "min_precision"
  )
  expect_error(
    acceptance_policy(2, 50, 10, .8, 0, NULL),
    "min_agreement_lower"
  )
  expect_error(
    acceptance_policy(.8, 50, 10, .8, 0, NULL, min_human_reliability_lower = 3),
    "min_human_reliability_lower"
  )
  expect_error(acceptance_policy(.8, 1, 10, .8, 0, NULL), "min_n")
  expect_error(acceptance_policy(.8, 50, 0, .8, 0, NULL), "min_class_n")
  expect_error(acceptance_policy(.8, 50, 10, .8, 2, NULL), "max_missing_rate")
  expect_error(acceptance_policy(.8, 50, 10, .8, 0, 2), "max_pairwise_disagreement")
  expect_error(
    acceptance_policy(.8, 50, 10, .8, 0, NULL, conf_level = 1), "conf_level"
  )
  expect_error(
    report(make_cal(), policy = list(), output = "md"),
    "acceptance_policy"
  )
})

test_that("required policy arguments cannot be omitted", {
  expect_error(acceptance_policy(), "min_agreement_lower")
  expect_error(acceptance_policy(.8, 50, 10, .8, 0), "max_pairwise_disagreement")
  expect_error(
    acceptance_policy(min_agreement_lower = .8, min_n = 50),
    "min_class_n"
  )
})

test_that("dependent thresholds are required with their gate", {
  expect_error(
    acceptance_policy(.8, 50, 10, .8, 0, max_pairwise_disagreement = 0.1),
    "min_stability_n.*required when.*max_pairwise_disagreement",
    class = "yourhonor_policy_error"
  )
  expect_error(
    acceptance_policy(
      .8, 50, 10, .8, 0, max_pairwise_disagreement = 0.1,
      min_stability_n = 10, min_stability_runs = 2
    ),
    "max_stability_missing_rate.*required when"
  )
  expect_error(
    acceptance_policy(.8, 50, 10, .8, 0, NULL, required_probes = "verbosity"),
    "min_probe_n.*required when.*required_probes",
    class = "yourhonor_policy_error"
  )
  expect_error(
    acceptance_policy(
      .8, 50, 10, .8, 0, NULL, required_probes = "verbosity",
      min_probe_n = 10, max_probe_flip_rate = .1,
      max_probe_directional_rate = .1
    ),
    "max_probe_missing_rate.*required when"
  )
})

test_that("dependent thresholds are rejected without their gate", {
  expect_error(
    acceptance_policy(.8, 50, 10, .8, 0, NULL, max_probe_flip_rate = 0.1),
    "max_probe_flip_rate.*supplied but.*required_probes",
    class = "yourhonor_policy_error"
  )
  expect_error(
    acceptance_policy(.8, 50, 10, .8, 0, NULL, min_probe_n = 10),
    "min_probe_n"
  )
  expect_error(
    acceptance_policy(
      .8, 50, 10, .8, 0, NULL, max_probe_directional_rate = 0.1
    ),
    "max_probe_directional_rate"
  )
  expect_error(
    acceptance_policy(.8, 50, 10, .8, 0, NULL, max_probe_missing_rate = 0),
    "max_probe_missing_rate"
  )
  expect_error(
    acceptance_policy(.8, 50, 10, .8, 0, NULL, min_stability_n = 10),
    "min_stability_n.*supplied but.*max_pairwise_disagreement",
    class = "yourhonor_policy_error"
  )
  expect_error(
    acceptance_policy(.8, 50, 10, .8, 0, NULL, min_stability_runs = 3),
    "min_stability_runs"
  )
  expect_error(
    acceptance_policy(.8, 50, 10, .8, 0, NULL, max_stability_missing_rate = 0),
    "max_stability_missing_rate"
  )
})

test_that("dependent threshold values are validated under their gate", {
  expect_error(
    acceptance_policy(
      .8, 50, 10, .8, 0, 0.1, min_stability_n = 0, min_stability_runs = 3,
      max_stability_missing_rate = 0
    ),
    "min_stability_n"
  )
  expect_error(
    acceptance_policy(
      .8, 50, 10, .8, 0, 0.1, min_stability_n = 10, min_stability_runs = 1,
      max_stability_missing_rate = 0
    ),
    "min_stability_runs"
  )
  expect_error(
    acceptance_policy(
      .8, 50, 10, .8, 0, 0.1, min_stability_n = 10, min_stability_runs = 3,
      max_stability_missing_rate = 2
    ),
    "max_stability_missing_rate"
  )
  expect_error(
    acceptance_policy(
      .8, 50, 10, .8, 0, NULL, required_probes = "verbosity",
      min_probe_n = 0, max_probe_flip_rate = .1,
      max_probe_directional_rate = .1, max_probe_missing_rate = 0
    ),
    "min_probe_n"
  )
  expect_error(
    acceptance_policy(
      .8, 50, 10, .8, 0, NULL, required_probes = "verbosity",
      min_probe_n = 10, max_probe_flip_rate = 2,
      max_probe_directional_rate = .1, max_probe_missing_rate = 0
    ),
    "max_probe_flip_rate"
  )
})
