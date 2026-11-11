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
  expect_match(paste(out, collapse = "\n"), "evidence is descriptive only")
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
  expect_match(out, "Assessment checks")
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
  expect_all(c("bias_position", "bias_position_direction",
    "bias_position_n", "bias_position_missing_rate") %in% rep$checks$check)
  expect_match(
    paste(format(rep), collapse = "\n"),
    "Required bias probes cleared the declared policy gates"
  )

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

test_that("a rendered value never equals the threshold it failed", {
  checks <- vctrs::vec_rbind(
    assessment_row(
      "calibration_missing_rate", "FAIL", 0.1004975, "<=", 0.1, "x"
    ),
    assessment_row("agreement_gwet_ac1", "FAIL", 0.8996, "lower >=", 0.9, "y"),
    assessment_row("usable_items", "PASS", 300, ">=", 50, "z"),
    assessment_row("bias_verbosity", "PASS", 0.3, "upper <=", 0.3, "w")
  )
  md <- format_check_lines(checks, style = "md")
  expect_match(md[[1]], "= 0.1005 \\(<= 0.1\\)")
  expect_match(md[[2]], "= 0.8996 \\(lower >= 0.9\\)")
  expect_match(md[[3]], "= 300 \\(>= 50\\)")
  expect_match(md[[4]], "= 0.3 \\(upper <= 0.3\\)")
  console <- format_check_lines(checks, style = "console")
  expect_match(
    console[[1]], "FAIL: calibration_missing_rate = 0.1005 \\(<= 0.1\\)"
  )

  # Neighbouring doubles that format() cannot separate at 15 significant
  # digits fall back to an exact rendering rather than reading as equal.
  below_one <- 1 - .Machine$double.eps
  pair <- fmt_check_pair(below_one, 1)
  expect_identical(pair$threshold, "1")
  expect_identical(pair$value, sprintf("%.17g", below_one))
  expect_false(identical(pair$value, pair$threshold))
  edge <- assessment_row("agreement", "FAIL", below_one, "lower >=", 1, "w")
  expect_match(
    format_check_lines(edge, style = "md"),
    "= 0.99999999999999978 \\(lower >= 1\\)",
    fixed = FALSE
  )
})

test_that("console reports list every class and the usable item count", {
  gold <- tibble::tibble(
    human_label = c(rep("C", 90), rep("I", 10)),
    judge_label = c(rep("C", 90), rep("C", 6), rep("I", 4))
  )
  cal <- calibrate_judge(
    gold, label_levels = c("I", "C"), coefficients = "gwet_ac1",
    n_boot = 50, seed = 1
  )
  out <- paste(format(report(cal), output = "console"), collapse = "\n")
  expect_match(out, "Judge evidence: n = 100 of 100")
  expect_match(out, "\"I\": support 10, precision 1.00, recall 0.40")
  expect_match(out, "\"C\": support 90, precision 0.94, recall 1.00")
  expect_false(grepl("Worst class", out))
})

test_that("an inconclusive probe outside the policy is mentioned", {
  judge <- function(d) ifelse(grepl("reviewers", d$input), NA, "C")
  bias <- make_bias(judge, probes = c("verbosity", "bandwagon"))
  expect_identical(bias$verdict[bias$probe == "bandwagon"], "INCONCLUSIVE")
  rep <- report(
    make_cal(), bias = bias,
    policy = make_policy(required_probes = "verbosity"), output = "md"
  )
  md <- paste(format(rep), collapse = "\n")
  expect_match(md, "Required bias probes cleared the declared policy gates")
  expect_match(md, "Outside the policy, bandwagon was INCONCLUSIVE")

  # A policy with no required probes leaves every probe outside it; the
  # unscored one is still named rather than described as showing no signal.
  none <- report(make_cal(), bias = bias, policy = make_policy(), output = "md")
  md <- paste(format(none), collapse = "\n")
  expect_match(md, "Bias probes were outside the policy")
  expect_match(md, "Outside the policy, bandwagon was INCONCLUSIVE")
  console <- paste(format(none, output = "console"), collapse = "\n")
  expect_match(console, "Outside the policy, bandwagon was INCONCLUSIVE")
  plain <- paste(format(report(make_cal(), bias = bias)), collapse = "\n")
  expect_match(plain, "At least one supplied bias probe was INCONCLUSIVE")
})

test_that("the console report renders a calibration without class metrics", {
  cal <- make_cal()
  cal$confusion_metrics <- NULL
  rep <- report(cal, policy = make_policy())
  expect_identical(rep$decision, "NOT_ASSESSED")
  expect_no_error(md <- format(rep, output = "md"))
  console <- paste(format(rep, output = "console"), collapse = "\n")
  expect_match(console, "Per-class evidence: not available")
  expect_match(
    paste(format(report(cal), output = "console"), collapse = "\n"),
    "Per-class evidence: not available"
  )
})
