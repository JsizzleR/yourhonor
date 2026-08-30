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

make_policy <- function(required_probes = character(0),
                        max_pairwise_disagreement = NULL,
                        min_human_reliability_lower = NULL) {
  acceptance_policy(
    min_agreement_lower = 0.9,
    min_n = 50,
    min_class_n = 15,
    min_recall = 0.8,
    max_missing_rate = 0,
    max_pairwise_disagreement = max_pairwise_disagreement,
    min_stability_n = 50,
    min_stability_runs = 3,
    max_stability_missing_rate = 0,
    required_probes = required_probes,
    min_probe_n = 20,
    max_probe_flip_rate = 0.1,
    max_probe_directional_rate = 0.1,
    max_probe_missing_rate = 0,
    min_human_reliability_lower = min_human_reliability_lower,
    coefficient = "gwet_ac1"
  )
}

test_that("reports are descriptive until a policy is supplied", {
  md <- report(make_cal(), output = "md")
  expect_s3_class(md, "yourhonor_report")
  expect_identical(attr(md, "decision"), "NOT_ASSESSED")
  expect_identical(attr(md, "checks")$status, "NOT_ASSESSED")
  txt <- paste(as.character(md), collapse = "\n")
  expect_match(txt, "Agreement with humans")
  expect_match(txt, "Assessment: \\*\\*NOT_ASSESSED\\*\\*")
  expect_match(txt, "Bias probes: \\*\\*NOT ASSESSED\\*\\*")
  expect_match(txt, "Test-retest stability: \\*\\*NOT ASSESSED\\*\\*")
})

test_that("supplied probes outside a policy are not described as clearing it", {
  bias <- probe_bias(
    tibble::tibble(input = paste("answer", 1:30)),
    function(d) ifelse(nchar(d$input) > 40, "C", "P"),
    probes = "verbosity", seed = 1, audit_id = "test-bundle"
  )
  md <- report(make_cal(), bias = bias, policy = make_policy(), output = "md")
  expect_match(paste(as.character(md), collapse = "\n"), "outside the policy")
  expect_false(grepl("cleared the declared policy", paste(md, collapse = "\n")))
})

test_that("console reports the explicit assessment state", {
  out <- cli::cli_fmt(report(make_cal(), output = "console"))
  expect_match(paste(out, collapse = "\n"), "yourhonor assessment: NOT_ASSESSED")
  expect_match(paste(out, collapse = "\n"), "Descriptive only")
})

test_that("a complete strong evidence bundle can pass a declared policy", {
  cal <- make_cal()
  data <- tibble::tibble(input = paste("answer", 1:60))
  flat_judge <- function(d) rep("C", nrow(d))
  bias <- probe_bias(
    data, flat_judge, probes = "verbosity", seed = 1,
    audit_id = "test-bundle"
  )
  stability <- test_retest(
    data, flat_judge, input_cols = "input", n_runs = 3,
    label_levels = c("I", "P", "C"),
    n_boot = 50, seed = 1, audit_id = "test-bundle"
  )
  policy <- make_policy(
    required_probes = "verbosity", max_pairwise_disagreement = 0.10
  )

  md <- report(
    cal, bias = bias, stability = stability, policy = policy, output = "md"
  )
  expect_identical(attr(md, "decision"), "PASS")
  expect_true(all(attr(md, "checks")$status == "PASS"))
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
  md <- report(cal, policy = policy, output = "md")
  checks <- attr(md, "checks")

  expect_gt(cal$agreement$conf_low, 0.9)
  expect_equal(cal$confusion_metrics$recall[cal$confusion_metrics$label == "I"], 0)
  expect_identical(attr(md, "decision"), "FAIL")
  expect_identical(checks$status[checks$check == "recall_I"], "FAIL")
})

test_that("required but omitted evidence is never treated as a pass", {
  policy <- make_policy(
    required_probes = "verbosity", max_pairwise_disagreement = 0.05
  )
  md <- report(make_cal(), policy = policy, output = "md")
  checks <- attr(md, "checks")
  expect_identical(attr(md, "decision"), "NOT_ASSESSED")
  expect_identical(checks$status[checks$check == "bias_verbosity"], "NOT_ASSESSED")
  expect_identical(
    checks$status[checks$check == "stability_pairwise_disagreement"],
    "NOT_ASSESSED"
  )
})

test_that("a missing policy coefficient is named instead of silently replaced", {
  policy <- make_policy()
  policy$coefficient <- "cohen_kappa"
  md <- report(make_cal(), policy = policy, output = "md")
  text <- paste(as.character(md), collapse = "\n")
  expect_match(text, "cohen_kappa.*not estimable")
  expect_identical(attr(md, "decision"), "NOT_ASSESSED")
})

test_that("unbound secondary evidence cannot satisfy a policy", {
  data <- tibble::tibble(input = paste("answer", 1:60))
  bias <- probe_bias(
    data, function(d) rep("C", nrow(d)), probes = "verbosity", seed = 1
  )
  md <- report(
    make_cal(), bias = bias,
    policy = make_policy(required_probes = "verbosity"), output = "md"
  )
  expect_identical(attr(md, "decision"), "NOT_ASSESSED")
  expect_match(paste(as.character(md), collapse = "\n"), "UNBOUND")
})

test_that("mismatched evidence bundle IDs error instead of being combined", {
  data <- tibble::tibble(input = paste("answer", 1:60))
  bias <- probe_bias(
    data, function(d) rep("C", nrow(d)), probes = "verbosity", seed = 1,
    audit_id = "different-bundle"
  )
  expect_error(report(make_cal(), bias = bias, output = "md"), "do not match")
})

test_that("small bias probes stay indeterminate despite no detected signal", {
  bias <- probe_bias(
    tibble::tibble(input = "answer"),
    function(d) rep("C", nrow(d)),
    probes = "verbosity", seed = 1, audit_id = "test-bundle"
  )
  md <- report(
    make_cal(), bias = bias,
    policy = make_policy(required_probes = "verbosity"), output = "md"
  )
  checks <- attr(md, "checks")
  expect_identical(attr(md, "decision"), "INDETERMINATE")
  expect_identical(checks$status[checks$check == "bias_verbosity"], "INDETERMINATE")
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
  md <- report(make_cal(), stability = stability, policy = policy, output = "md")
  checks <- attr(md, "checks")
  expect_identical(attr(md, "decision"), "FAIL")
  expect_identical(
    checks$status[checks$check == "stability_missing_rate"], "FAIL"
  )
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
  md <- report(make_cal(), stability = stability, policy = policy, output = "md")
  checks <- attr(md, "checks")
  expect_identical(attr(md, "decision"), "INDETERMINATE")
  expect_identical(checks$status[checks$check == "stability_items"], "INDETERMINATE")
  expect_identical(checks$status[checks$check == "stability_runs"], "INDETERMINATE")
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
  md <- report(cal, policy = make_policy(), output = "md")
  expect_identical(attr(md, "decision"), "INDETERMINATE")
})

test_that("a zero-width perfect-sample bootstrap cannot clear policy", {
  labels <- rep(c("I", "P", "C"), each = 100)
  cal <- calibrate_judge(
    tibble::tibble(human_label = labels, judge_label = labels),
    label_levels = c("I", "P", "C"), coefficients = "gwet_ac1",
    n_boot = 50, seed = 1
  )
  md <- report(cal, policy = make_policy(), output = "md")
  checks <- attr(md, "checks")
  expect_true(cal$agreement$boundary_degenerate)
  expect_identical(attr(md, "decision"), "INDETERMINATE")
  expect_identical(
    checks$status[checks$check == "agreement_gwet_ac1"], "INDETERMINATE"
  )
})

test_that("policy confidence level must match the evidence", {
  md <- report(
    make_cal(conf_level = 0.90), policy = make_policy(), output = "md"
  )
  checks <- attr(md, "checks")
  expect_identical(attr(md, "decision"), "INDETERMINATE")
  expect_identical(
    checks$status[checks$check == "calibration_confidence_level"],
    "INDETERMINATE"
  )
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
  md <- report(make_cal(), bias = bias, policy = policy, output = "md")
  checks <- attr(md, "checks")
  expect_equal(bias$n_scored, 10)
  expect_equal(bias$missing_rate, 0.9)
  expect_identical(attr(md, "decision"), "FAIL")
  expect_identical(
    checks$status[checks$check == "bias_verbosity_missing_rate"], "FAIL"
  )
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
  md <- report(cal, policy = policy, output = "md")
  checks <- attr(md, "checks")
  expect_true("human_reliability" %in% checks$check)
  expect_identical(checks$status[checks$check == "human_reliability"], "FAIL")
  expect_identical(attr(md, "decision"), "FAIL")

  single_policy <- make_policy(min_human_reliability_lower = 0.5)
  single <- report(make_cal(), policy = single_policy, output = "md")
  expect_identical(attr(single, "decision"), "NOT_ASSESSED")
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
  md <- report(
    cal,
    policy = make_policy(required_probes = "verbosity"),
    output = "md"
  )
  expect_identical(attr(md, "decision"), "FAIL")
  expect_true(any(attr(md, "checks")$status == "NOT_ASSESSED"))
})

test_that("policy controls validate", {
  expect_error(
    acceptance_policy(.8, 50, 10, .8, 0, NULL, required_probes = "unknown"),
    "required_probes"
  )
  expect_error(
    acceptance_policy(.8, 50, 10, c(I = 1.2), 0, NULL),
    "min_recall"
  )
  expect_error(
    report(make_cal(), policy = list(), output = "md"),
    "acceptance_policy"
  )
})

test_that("markdown reports print as plain lines", {
  md <- report(make_cal(), output = "md")
  out <- capture.output(print(md))
  expect_match(paste(out, collapse = "\n"), "Judge evidence")
})
