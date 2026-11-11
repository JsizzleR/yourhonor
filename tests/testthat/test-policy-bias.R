# Bias gates: probe size, confidence level, missingness and unscored items.

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

test_that("a weak position bias cannot pass by tying out across repeats", {
  # On each call the judge takes the first slot with probability 0.2 and
  # otherwise picks the better candidate. With two repeats most of the bias
  # lands in within-condition ties, which leave items unscored with no
  # missing judgment, so the scored items alone looked clean.
  weakly_first <- function(d) {
    better <- ifelse(grepl("good", d$.candidate_1), "A", "B")
    ifelse(stats::runif(nrow(d)) < 0.2, "A", better)
  }
  bias <- probe_position(
    tie_pairs(600), weakly_first, "candidate_a", "candidate_b",
    context_cols = "context", repeats = 2, seed = 1,
    audit_id = "test-bundle"
  )
  rep <- report(
    make_cal(), bias = bias,
    policy = make_policy(required_probes = "position", min_probe_n = 50),
    output = "md"
  )
  expect_gt(bias$n_unscored, 100)
  expect_identical(bias$missing_rate, 0)
  expect_lt(bias$conf_high, 0.1)
  expect_identical(check_status(rep, "bias_position_missing_rate"), "PASS")
  expect_identical(check_status(rep, "bias_position_n"), "PASS")
  expect_identical(check_status(rep, "bias_position"), "INDETERMINATE")
  expect_identical(check_status(rep, "bias_position_direction"), "INDETERMINATE")
  flip <- rep$checks[rep$checks$check == "bias_position", ]
  expect_equal(
    flip$value,
    stats::binom.test(
      bias$n_discordant + bias$n_unscored, bias$n_input
    )$conf.int[[2]]
  )
  expect_match(flip$reason, paste(bias$n_unscored, "unscored items"))
  direction <- rep$checks[rep$checks$check == "bias_position_direction", ]
  expect_gt(direction$value, bias$directional_conf_high)
  expect_lte(direction$value, flip$value)
})

test_that("a random position judge's ties widen the flip-rate bound", {
  coin <- function(d) sample(c("A", "B"), nrow(d), replace = TRUE)
  bias <- probe_position(
    tie_pairs(40), coin, "candidate_a", "candidate_b",
    context_cols = "context", repeats = 2, seed = 3,
    audit_id = "test-bundle"
  )
  rep <- report(
    make_cal(), bias = bias,
    policy = make_policy(required_probes = "position", min_probe_n = 5),
    output = "md"
  )
  expect_gt(bias$unscored_rate, 0.5)
  expect_identical(bias$missing_rate, 0)
  flip <- rep$checks[rep$checks$check == "bias_position", ]
  expect_gt(flip$value, bias$conf_high)
  expect_equal(
    flip$value,
    stats::binom.test(
      bias$n_discordant + bias$n_unscored, bias$n_input
    )$conf.int[[2]]
  )
  expect_false(flip$status == "PASS")
})

test_that("bias counts that are missing or do not add up are unusable", {
  policy <- make_policy(required_probes = "verbosity")
  bias <- make_bias()
  missing_count <- bias
  missing_count$n_unscored <- NA_integer_
  # Five discordant items hidden by a negative unscored count would bound
  # zero flips if the counts were taken on trust.
  inconsistent <- bias
  inconsistent$n_discordant <- 5L
  inconsistent$n_unscored <- -5L
  fractional <- bias
  fractional$n_scored <- bias$n_scored - 0.5
  fractional$n_unscored <- 0.5
  for (bad in list(missing_count, inconsistent, fractional)) {
    rep <- report(make_cal(), bias = bad, policy = policy, output = "md")
    for (check in c("bias_verbosity", "bias_verbosity_direction")) {
      row <- rep$checks[rep$checks$check == check, ]
      expect_identical(row$status, "INDETERMINATE")
      expect_identical(row$reason, "Interval is unavailable.")
    }
  }
})

test_that("pointwise items lost to tolerated abstention count as flips", {
  # Five items lose their treatment judgment: within a 5% missingness
  # tolerance, but the flip-rate bound must allow that each one flipped.
  abstains <- function(d) {
    ifelse(grepl("^answer [1-5]\n", d$input), NA_character_, "C")
  }
  bias <- probe_bias(
    tibble::tibble(input = paste("answer", 1:100)), abstains,
    probes = "verbosity", seed = 1, audit_id = "test-bundle"
  )
  policy <- acceptance_policy(
    min_agreement_lower = 0.9, min_n = 50, min_class_n = 15,
    min_recall = 0.8, max_missing_rate = 0,
    max_pairwise_disagreement = NULL,
    required_probes = "verbosity", min_probe_n = 20,
    max_probe_flip_rate = 0.1, max_probe_directional_rate = 0.1,
    max_probe_missing_rate = 0.05,
    coefficient = "gwet_ac1"
  )
  rep <- report(make_cal(), bias = bias, policy = policy, output = "md")
  expect_equal(bias$n_unscored, 5)
  expect_equal(bias$missing_rate, 0.025)
  expect_lt(bias$conf_high, 0.1)
  expect_identical(check_status(rep, "bias_verbosity_missing_rate"), "PASS")
  expect_identical(check_status(rep, "bias_verbosity"), "INDETERMINATE")
  expect_identical(check_status(rep, "bias_verbosity_direction"), "INDETERMINATE")
})

test_that("a fully scored probe's bias checks read the row's own bounds", {
  bias <- make_bias(judge = length_judge, n = 60)
  position <- probe_position(
    tie_pairs(60), function(d) ifelse(grepl("good", d$.candidate_1), "A", "B"),
    "candidate_a", "candidate_b", context_cols = "context",
    seed = 1, audit_id = "test-bundle"
  )
  combined <- combine_bias(bias, position)
  rep <- report(
    make_cal(), bias = combined,
    policy = make_policy(required_probes = c("verbosity", "position")),
    output = "md"
  )
  for (probe in c("verbosity", "position")) {
    row <- combined[combined$probe == probe, ]
    expect_identical(row$n_unscored, 0L)
    flip <- rep$checks[rep$checks$check == paste0("bias_", probe), ]
    direction <- rep$checks[
      rep$checks$check == paste0("bias_", probe, "_direction"),
    ]
    expect_identical(flip$value, row$conf_high)
    expect_identical(direction$value, row$directional_conf_high)
    expect_no_match(flip$reason, "unscored")
  }
  position_check <- rep$checks[rep$checks$check == "bias_position", ]
  expect_identical(position_check$status, "PASS")
})

test_that("an undersized required probe is a prerequisite failure", {
  bias <- probe_bias(
    tibble::tibble(input = paste("answer", 1:3)), length_judge,
    probes = "verbosity", seed = 1, audit_id = "test-bundle"
  )
  expect_identical(bias$flip_rate, 1)
  rep <- report(
    make_cal(), bias = bias,
    policy = make_policy(required_probes = "verbosity"), output = "md"
  )
  expect_identical(rep$decision, "INDETERMINATE")
  expect_identical(check_status(rep, "bias_verbosity_n"), "INDETERMINATE")
  row <- rep$checks[rep$checks$check == "bias_verbosity", ]
  expect_identical(row$status, "INDETERMINATE")
  expect_true(is.na(row$value))
  expect_match(row$reason, "below the declared minimum.*not assessed")
  expect_false("bias_verbosity_direction" %in% rep$checks$check)
  expect_false(any(rep$checks$status == "FAIL"))
})

test_that("a probe at another confidence level keeps rate and count checks", {
  bias <- probe_bias(
    tibble::tibble(input = paste("answer", 1:60)), flat_judge,
    probes = "verbosity", seed = 1, conf_level = 0.9,
    audit_id = "test-bundle"
  )
  rep <- report(
    make_cal(), bias = bias,
    policy = make_policy(required_probes = "verbosity"), output = "md"
  )
  expect_identical(check_status(rep, "bias_verbosity_missing_rate"), "PASS")
  expect_identical(check_status(rep, "bias_verbosity_n"), "PASS")
  expect_identical(
    check_status(rep, "bias_verbosity_confidence_level"), "INDETERMINATE"
  )
  expect_false("bias_verbosity" %in% rep$checks$check)
  expect_false("bias_verbosity_direction" %in% rep$checks$check)
})

test_that("min_probe_n counts scored items, not condition pairs", {
  # 30 pairs at three repeats make 90 condition pairs but 30 items; the
  # floor is assessed on the items, as the documentation says.
  better <- function(d) ifelse(grepl("good", d$.candidate_1), "A", "B")
  bias <- probe_position(
    tie_pairs(30), better, "candidate_a", "candidate_b",
    context_cols = "context", repeats = 3, seed = 1,
    audit_id = "test-bundle"
  )
  expect_identical(bias$n_comparisons, 90L)
  expect_identical(bias$n_scored, 30L)
  rep <- report(
    make_cal(), bias = bias,
    policy = make_policy(required_probes = "position", min_probe_n = 50),
    output = "md"
  )
  n_row <- rep$checks[rep$checks$check == "bias_position_n", ]
  expect_identical(n_row$status, "INDETERMINATE")
  expect_equal(n_row$value, 30)
  expect_equal(n_row$threshold, 50)
})
