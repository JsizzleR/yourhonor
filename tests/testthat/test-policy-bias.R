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
  expect_none(rep$checks$status == "FAIL")
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

# Items 1-30 flip from P to C under the verbosity filler; items 31-40 stay
# P; on the rest the judge abstains in the perturbed condition when
# `abstain` is TRUE (they would have stayed P).
selective_bias <- function(n_flip = 30, n_stay = 10, n_abstain = 60) {
  judge <- function(d) {
    item <- as.integer(sub("^answer ([0-9]+).*$", "\\1", d$input))
    perturbed <- nchar(d$input) > 40
    out <- ifelse(item <= n_flip & perturbed, "C", "P")
    out[item > n_flip + n_stay & perturbed] <- NA_character_
    out
  }
  n <- n_flip + n_stay + n_abstain
  probe_bias(
    tibble::tibble(input = paste("answer", seq_len(n))), judge,
    probes = "verbosity", seed = 1, audit_id = "test-bundle"
  )
}

bias_gate_policy <- function(flip, direction, missing = 0.5) {
  acceptance_policy(
    min_agreement_lower = 0.9, min_n = 50, min_class_n = 15,
    min_recall = 0.8, max_missing_rate = 0,
    max_pairwise_disagreement = NULL,
    required_probes = "verbosity", min_probe_n = 20,
    max_probe_flip_rate = flip, max_probe_directional_rate = direction,
    max_probe_missing_rate = missing, coefficient = "gwet_ac1"
  )
}

bias_rows <- function(bias, policy) {
  rows <- single_probe_checks("verbosity", make_cal(), bias, policy)
  stats::setNames(rows, vapply(rows, `[[`, "", "check"))
}

test_that("unscored items alone cannot FAIL a probe's interval checks", {
  bias <- selective_bias()
  expect_identical(bias$n_scored, 40L)
  expect_identical(bias$n_discordant, 30L)
  expect_identical(bias$n_unscored, 60L)
  pol <- bias_gate_policy(flip = 0.5, direction = 0.4)
  # Over the scored items alone both lower bounds exceed their thresholds.
  expect_gt(bias$conf_low, 0.5)
  expect_gt(bias$directional_conf_low, 0.4)
  rows <- bias_rows(bias, pol)
  # Unflipped, the unscored items put 30 flips over 100 items: [0.21, 0.40].
  expect_identical(rows$bias_verbosity$status, "INDETERMINATE")
  expect_match(
    rows$bias_verbosity$reason,
    paste("The lower bound counts the 60 unscored items with a missing",
          "judgment as unflipped over 100 items."),
    fixed = TRUE
  )
  # 30 of them flipping the other way would cancel the direction entirely.
  expect_identical(rows$bias_verbosity_direction$status, "INDETERMINATE")
  expect_match(
    rows$bias_verbosity_direction$reason, "cancelling the observed direction",
    fixed = TRUE
  )
  expect_identical(rows$bias_verbosity_missing_rate$status, "PASS")
  expect_none(vapply(rows, `[[`, "", "status") == "FAIL")
})

test_that("a biased probe still FAILs however its unscored items resolve", {
  # 60 of 90 scored items flip, in one direction; 10 are unscored.
  bias <- selective_bias(n_flip = 60, n_stay = 30, n_abstain = 10)
  rows <- bias_rows(bias, bias_gate_policy(flip = 0.4, direction = 0.2))
  expect_identical(rows$bias_verbosity$status, "FAIL")
  expect_identical(rows$bias_verbosity_direction$status, "FAIL")
  # The same judge with nothing unscored fails too, on its own bounds.
  complete <- selective_bias(n_flip = 60, n_stay = 40, n_abstain = 0)
  rows <- bias_rows(complete, bias_gate_policy(flip = 0.4, direction = 0.2))
  expect_identical(rows$bias_verbosity$status, "FAIL")
  expect_identical(rows$bias_verbosity_direction$status, "FAIL")
  expect_no_match(rows$bias_verbosity$reason, "unscored")
})

test_that("the best-case bounds are the least over every resolution of the unscored items", {
  lows <- function(positive, negative, n, conf_level) {
    c(
      flip = exact_binom_interval(positive + negative, n, conf_level)[[1]],
      directional = directional_magnitude_interval(
        positive, negative, n, conf_level
      )[[1]]
    )
  }
  for (conf_level in c(0.8, 0.95)) {
    for (n_scored in c(5, 12)) {
      for (n_unscored in c(2, 7)) {
        n_input <- n_scored + n_unscored
        for (positive in 0:n_scored) {
          for (negative in 0:(n_scored - positive)) {
            observed <- lows(positive, negative, n_scored, conf_level)
            # Only a position row can have ties.
            row <- tibble::tibble(
              probe = "position",
              conf_low = observed[["flip"]],
              directional_conf_low = observed[["directional"]],
              statistic = positive, n_directional_items = positive + negative,
              n_discordant = positive + negative, n_scored = n_scored,
              n_input = n_input, n_unscored = n_unscored,
              n_unscored_tied = 0L, conf_level = conf_level
            )
            # Tied items stay out; only the rest are completed.
            for (tied in unique(c(0L, 1L, n_unscored))) {
              row$n_unscored_tied <- tied
              best <- best_case_lower_bounds(row)
              open <- n_unscored - tied
              least <- if (open == 0L) {
                observed
              } else {
                least <- c(flip = Inf, directional = Inf)
                for (up in 0:open) {
                  for (down in 0:(open - up)) {
                    least <- pmin(least, lows(
                      positive + up, negative + down, n_input - tied,
                      conf_level
                    ))
                  }
                }
                least
              }
              expect_equal(best$flip, least[["flip"]], tolerance = 0)
              expect_equal(
                best$directional, least[["directional"]], tolerance = 0
              )
            }
          }
        }
      }
    }
  }
})

test_that("a position judge cannot dodge a FAIL through within-condition ties", {
  # Each call takes the first slot with probability 0.5, otherwise the
  # better candidate: at repeats = 2 its bias splits votes into ties, with
  # no judgment missing.
  pairs <- tibble::tibble(a = paste("good", 1:100), b = paste("bad", 1:100))
  judge <- function(d) {
    ifelse(stats::runif(nrow(d)) < 0.5, "A",
           ifelse(grepl("good", d$.candidate_1), "A", "B"))
  }
  position <- probe_position(
    pairs, judge, "a", "b", seed = 5, audit_id = "test-bundle", repeats = 2
  )
  expect_identical(position$missing_rate, 0)
  expect_gt(position$n_unscored, 0L)
  expect_identical(position$n_unscored_tied, position$n_unscored)
  expect_gt(position$conf_low, 0.2)
  pol <- acceptance_policy(
    min_agreement_lower = 0.9, min_n = 50, min_class_n = 15,
    min_recall = 0.8, max_missing_rate = 0,
    max_pairwise_disagreement = NULL,
    required_probes = "position", min_probe_n = 10,
    max_probe_flip_rate = 0.2, max_probe_directional_rate = 0.2,
    max_probe_missing_rate = 0.05, coefficient = "gwet_ac1"
  )
  rows <- single_probe_checks("position", make_cal(), position, pol)
  rows <- stats::setNames(rows, vapply(rows, `[[`, "", "check"))
  # Ties stay out of the lower bound, which is the scored items' own.
  expect_identical(best_case_lower_bounds(position)$flip, position$conf_low)
  expect_identical(rows$bias_position$status, "FAIL")
  expect_no_match(rows$bias_position$reason, "lower bound")
  # A tie count that does not add up gives no best case to assess.
  bad <- position
  bad$n_unscored_tied <- position$n_unscored + 1L
  expect_identical(
    best_case_lower_bounds(bad), list(flip = NA_real_, directional = NA_real_)
  )
})

test_that("pointwise probes have no ties and position ties are counted", {
  expect_identical(make_bias()$n_unscored_tied, 0L)
  abstains <- function(d) {
    ifelse(grepl("^answer [1-5]\n", d$input), NA_character_, "C")
  }
  pointwise <- probe_bias(
    tibble::tibble(input = paste("answer", 1:20)), abstains,
    probes = "verbosity", seed = 1, audit_id = "test-bundle"
  )
  expect_gt(pointwise$n_unscored, 0L)
  expect_identical(pointwise$n_unscored_tied, 0L)
  # Pair 1 ties in the original order; pair 2 is missing there entirely.
  judge <- function(d) {
    out <- rep("A", nrow(d))
    first <- d$.candidate_1 == "x1" | d$.candidate_2 == "x1"
    original <- d$.candidate_1 == "x1" | d$.candidate_1 == "x2"
    out[first & original] <- c("A", "B")
    out[(d$.candidate_1 == "x2") ] <- NA_character_
    out
  }
  pairs <- tibble::tibble(a = paste0("x", 1:6), b = paste0("y", 1:6))
  position <- probe_position(pairs, judge, "a", "b", seed = 1,
                             audit_id = "test-bundle", repeats = 2)
  ev <- attr(position, "item_evidence")$position
  unscored <- is.na(ev$flipped)
  expect_identical(position$n_unscored, sum(unscored))
  expect_identical(position$n_unscored_tied, 1L)
})

test_that("best-case bounds of a fully scored row are the row's own", {
  bias <- make_bias(judge = length_judge, n = 60)
  best <- best_case_lower_bounds(bias)
  expect_identical(best$flip, bias$conf_low)
  expect_identical(best$directional, bias$directional_conf_low)
})

test_that("bias counts of the wrong type give unusable bounds, not an error", {
  bias <- selective_bias()
  for (field in c("statistic", "n_directional_items", "n_input", "n_unscored")) {
    for (value in list("30", NA, c(30L, 31L))) {
      bad <- tibble::as_tibble(bias)
      bad[[field]] <- if (length(value) == 1L) value else list(value)
      expect_identical(
        best_case_lower_bounds(bad), list(flip = NA_real_, directional = NA_real_)
      )
      expect_identical(
        worst_case_upper_bounds(bad), list(flip = NA_real_, directional = NA_real_)
      )
    }
  }
})

test_that("a non-finite observed lower bound is not repaired by the best case", {
  bias <- selective_bias()
  bias$conf_low <- Inf
  bias$directional_conf_low <- NA_real_
  rows <- bias_rows(bias, bias_gate_policy(flip = 0.5, direction = 0.4))
  expect_identical(rows$bias_verbosity$status, "INDETERMINATE")
  expect_match(rows$bias_verbosity$reason, "^Interval is unavailable\\.")
  expect_no_match(rows$bias_verbosity$reason, "lower bound")
  expect_identical(rows$bias_verbosity_direction$status, "INDETERMINATE")
})

test_that("an unavailable probe missing rate is INDETERMINATE, not FAIL", {
  bias <- make_bias()
  bias$missing_rate <- NA_real_
  rows <- bias_rows(bias, make_policy(required_probes = "verbosity"))
  expect_identical(rows$bias_verbosity_missing_rate$status, "INDETERMINATE")
  expect_identical(
    rows$bias_verbosity_missing_rate$reason, "Missing rate is unavailable."
  )
  bias <- make_bias()
  bias$n_scored <- NA_integer_
  rows <- bias_rows(bias, make_policy(required_probes = "verbosity"))
  expect_identical(rows$bias_verbosity_n$status, "INDETERMINATE")
})

test_that("a tie in a fully judged display order stays decided when the other order has a hole", {
  # Items 1-60 always take slot 1: a flip. Items 61-100 split A/B across the
  # two original-order repeats, a tie no answer can break; with `abstain`
  # one of their swapped repeats is missing too.
  judge_for <- function(abstain) {
    function(d) {
      key <- paste(d$.candidate_1, d$.candidate_2)
      occurrence <- stats::ave(seq_along(key), key, FUN = seq_along)
      original <- grepl("^good", d$.candidate_1)
      item <- as.integer(sub("\\D+", "", d$.candidate_1))
      late <- item > 60
      out <- rep("A", nrow(d))
      out[late & original & occurrence == 2] <- "B"
      out[late & !original] <- "B"
      if (abstain) out[late & !original & occurrence == 2] <- NA_character_
      out
    }
  }
  pairs <- tibble::tibble(a = paste("good", 1:100), b = paste("bad", 1:100))
  pol <- acceptance_policy(
    min_agreement_lower = 0.9, min_n = 50, min_class_n = 15,
    min_recall = 0.8, max_missing_rate = 0,
    max_pairwise_disagreement = NULL,
    required_probes = "position", min_probe_n = 10,
    max_probe_flip_rate = 0.5, max_probe_directional_rate = 0.5,
    max_probe_missing_rate = 0.1, coefficient = "gwet_ac1"
  )
  for (abstain in c(FALSE, TRUE)) {
    position <- probe_position(
      pairs, judge_for(abstain), "a", "b", seed = 1,
      audit_id = "test-bundle", repeats = 2
    )
    expect_identical(position$n_unscored, 40L)
    expect_identical(position$n_unscored_tied, 40L)
    expect_identical(best_case_lower_bounds(position)$flip, position$conf_low)
    rows <- single_probe_checks("position", make_cal(), position, pol)
    rows <- stats::setNames(rows, vapply(rows, `[[`, "", "check"))
    expect_identical(rows$bias_position$status, "FAIL")
    expect_identical(rows$bias_position_direction$status, "FAIL")
  }
  # A tie in an order with a missing repeat can still be broken: at
  # repeats = 3 the late items read A, B, NA in the original order, which an
  # answer for the NA decides, so they are completed, not tied.
  breakable <- function(d) {
    key <- paste(d$.candidate_1, d$.candidate_2)
    occurrence <- stats::ave(seq_along(key), key, FUN = seq_along)
    original <- grepl("^good", d$.candidate_1)
    item <- as.integer(sub("\\D+", "", d$.candidate_1))
    late <- item > 60
    out <- rep("A", nrow(d))
    out[late & original & occurrence == 2] <- "B"
    out[late & original & occurrence == 3] <- NA_character_
    out[late & !original] <- "B"
    out
  }
  position <- probe_position(
    pairs, breakable, "a", "b", seed = 1, audit_id = "test-bundle",
    repeats = 3
  )
  expect_identical(position$n_unscored, 40L)
  expect_identical(position$n_unscored_tied, 0L)
  expect_lt(best_case_lower_bounds(position)$flip, 0.5)
  expect_gt(position$conf_low, 0.5)
  rows <- single_probe_checks("position", make_cal(), position, pol)
  rows <- stats::setNames(rows, vapply(rows, `[[`, "", "check"))
  expect_identical(rows$bias_position$status, "INDETERMINATE")
})

test_that("a pointwise row cannot claim ties to dodge its best case", {
  bias <- selective_bias()
  expect_gt(bias$n_unscored, 0L)
  rows <- bias_rows(bias, bias_gate_policy(flip = 0.5, direction = 0.4))
  expect_identical(rows$bias_verbosity$status, "INDETERMINATE")
  bad <- bias
  bad$n_unscored_tied <- bias$n_unscored
  expect_identical(
    best_case_lower_bounds(bad), list(flip = NA_real_, directional = NA_real_)
  )
  rows <- bias_rows(bad, bias_gate_policy(flip = 0.5, direction = 0.4))
  expect_identical(rows$bias_verbosity$status, "INDETERMINATE")
  expect_identical(rows$bias_verbosity_direction$status, "INDETERMINATE")
})

test_that("an unavailable probe sample size says so, not that it is undersized", {
  bias <- make_bias()
  bias$n_scored <- NA_integer_
  rows <- bias_rows(bias, make_policy(required_probes = "verbosity"))
  prerequisite <- rows[[length(rows)]]
  expect_identical(prerequisite$status, "INDETERMINATE")
  expect_match(prerequisite$reason, "Sample size is unavailable", fixed = TRUE)
  expect_no_match(prerequisite$reason, "below the declared minimum")
})
