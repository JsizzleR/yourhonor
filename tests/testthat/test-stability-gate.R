# Policy-level tests of the stability gate under missing judgments: an
# abstention may widen the assessed interval either way (PASS or FAIL ->
# INDETERMINATE) but can never clear it, and can never cause a FAIL.

gate_policy <- function(max_missing = 0.1, min_n = 50, min_runs = 3,
                        max_disagreement = 0.1) {
  acceptance_policy(
    min_agreement_lower = 0.5, min_n = 50, min_class_n = 5, min_recall = 0.5,
    max_missing_rate = 0.1, max_pairwise_disagreement = max_disagreement,
    min_stability_n = min_n, min_stability_runs = min_runs,
    max_stability_missing_rate = max_missing
  )
}

gate_calibration <- function(audit_id = "gate") {
  structure(list(audit_id = audit_id), class = "yourhonor_calibration")
}

gate_rows <- function(stability, policy) {
  rows <- stability_checks(gate_calibration(stability$audit_id), stability, policy)
  stats::setNames(rows, vapply(rows, `[[`, "", "check"))
}

# The rows the stored values would get if the policy trusted them, so the
# interval's own guards stay tested. The policy does not trust them: an
# object whose stored values do not match their rebuild reads INDETERMINATE
# on every check, which this asserts first.
stored_rows <- function(stability, policy) {
  if (!is.null(verify_stability_evidence(stability)$reason)) {
    rows <- gate_rows(stability, policy)
    testthat::expect_identical(
      unname(vapply(rows, `[[`, "", "status")), rep("INDETERMINATE", 4L)
    )
  }
  local_mocked_bindings(
    verify_stability_evidence = function(stability) {
      list(stability = stability, reason = NULL)
    }
  )
  gate_rows(stability, policy)
}

# On items 1-25 the judge deviates in run 5 only: a different label, or NA.
deviating_judge <- function(abstain) {
  calls <- 0
  function(d) {
    calls <<- calls + 1
    item <- as.integer(sub("answer ", "", d$input, fixed = TRUE))
    out <- ifelse(item %% 2L == 0L, "C", "P")
    unstable <- item <= 25
    if (calls == 5) {
      out[unstable] <- if (abstain) NA else ifelse(out[unstable] == "C", "P", "C")
    }
    out
  }
}

deviating_retest <- function(abstain) {
  test_retest(
    tibble::tibble(input = paste("answer", 1:100)), deviating_judge(abstain),
    input_cols = "input", n_runs = 5, label_levels = c("P", "C"),
    n_boot = 200, seed = 1, audit_id = "gate"
  )
}

# 300 items, 3 epochs; the last 45 items are unstable: answered inconsistently
# (C, P, I), or answered once and then NA twice.
hidden_vitals <- function(abstain) {
  ids <- sprintf("i%03d", 1:300)
  rows <- expand.grid(id = ids, epoch = 1:3, stringsAsFactors = FALSE)
  rows$input <- paste("q", rows$id)
  k <- match(rows$id, ids)
  mild <- k <= 20
  hidden <- k > 255
  rows$score <- "C"
  rows$score[mild & rows$epoch == 3] <- "P"
  if (abstain) {
    rows$score[hidden & rows$epoch > 1] <- NA
  } else {
    rows$score[hidden] <- c("C", "P", "I")[rows$epoch[hidden]]
  }
  stability_vitals(
    rows, content_cols = "input", label_levels = c("I", "P", "C"),
    n_boot = 200, seed = 1, audit_id = "gate"
  )
}

test_that("a judge that flips its unstable items is INDETERMINATE (control)", {
  st <- deviating_retest(abstain = FALSE)
  expect_equal(st$missing_rate, 0)
  expect_identical(
    st$pairwise_disagreement_worst_case_high,
    st$pairwise_disagreement_conf_high
  )
  rows <- gate_rows(st, gate_policy())
  expect_identical(rows$stability_pairwise_disagreement$status, "INDETERMINATE")
  expect_equal(
    rows$stability_pairwise_disagreement$value,
    st$pairwise_disagreement_conf_high
  )
})

test_that("a judge that abstains on its unstable items no longer passes", {
  st <- deviating_retest(abstain = TRUE)
  expect_equal(st$missing_rate, 0.05)
  expect_equal(st$pairwise_disagreement, 0)
  # Each of the 25 items has 4 of its 10 pairs involving the missing run.
  expect_equal(st$pairwise_disagreement_worst_case, 25 * 0.4 / 100)
  rows <- gate_rows(st, gate_policy())
  disagreement <- rows$stability_pairwise_disagreement
  expect_identical(disagreement$status, "INDETERMINATE")
  expect_gt(disagreement$value, 0.1)
  expect_match(disagreement$reason, "25 missing judgments", fixed = TRUE)
  expect_match(disagreement$reason, "over all 100 items", fixed = TRUE)
  # The other checks are unchanged.
  expect_identical(rows$stability_items$status, "PASS")
  expect_identical(rows$stability_missing_rate$status, "PASS")
  expect_equal(st$any_flip_rate_worst_case, 0.25)
  expect_equal(st$n_items_complete, 75)
  expect_equal(st$any_flip_rate_at_n_runs, 0)
  printed <- paste(cli::cli_fmt(print(st)), collapse = " ")
  expect_match(printed, "Worst case, counting missing judgments", fixed = TRUE)
})

test_that("an inconsistent judge still FAILs and an abstaining one is INDETERMINATE", {
  flip <- hidden_vitals(abstain = FALSE)
  abst <- hidden_vitals(abstain = TRUE)
  pol <- gate_policy(max_missing = 0.1, min_n = 200)
  expect_identical(
    gate_rows(flip, pol)$stability_pairwise_disagreement$status, "FAIL"
  )
  expect_equal(abst$n_items_scored, 255)
  expect_equal(abst$missing_rate, 0.1)
  rows <- gate_rows(abst, pol)
  expect_identical(rows$stability_pairwise_disagreement$status, "INDETERMINATE")
  expect_identical(rows$stability_items$status, "PASS")
  expect_identical(rows$stability_missing_rate$status, "PASS")
  # The worst case lies wholly above the threshold, yet it is not a FAIL:
  # that needs the observed lower bound above it.
  expect_gt(abst$pairwise_disagreement_worst_case, 0.1)
  expect_lte(abst$pairwise_disagreement_conf_low, 0.1)
  # 45 items with one judgment count as wholly disagreeing.
  expect_equal(
    abst$pairwise_disagreement_worst_case, (20 * 2 / 3 + 45) / 300
  )
})

test_that("the abstaining hidden-instability judge is not accepted by report()", {
  ids <- sprintf("i%03d", 1:300)
  gold <- tibble::tibble(
    id = ids, human_label = rep(c("C", "P", "I"), 100),
    judge_label = rep(c("C", "P", "I"), 100)
  )
  cal <- calibrate_judge(
    gold, id_col = "id", label_levels = c("I", "P", "C"), n_boot = 200,
    seed = 1, audit_id = "gate"
  )
  pol <- acceptance_policy(
    min_agreement_lower = 0.5, min_n = 50, min_class_n = 10, min_recall = 0.5,
    max_missing_rate = 0.1, max_pairwise_disagreement = 0.1,
    min_stability_n = 200, min_stability_runs = 3,
    max_stability_missing_rate = 0.1
  )
  expect_identical(
    report(cal, stability = hidden_vitals(TRUE), policy = pol)$decision,
    "INDETERMINATE"
  )
  expect_identical(
    report(cal, stability = hidden_vitals(FALSE), policy = pol)$decision,
    "FAIL"
  )
})

test_that("a clean judge still passes and its bound is the observed one", {
  judge <- function(d) {
    item <- as.integer(sub("answer ", "", d$input, fixed = TRUE))
    ifelse(item %% 2L == 0L, "C", "P")
  }
  st <- test_retest(
    tibble::tibble(input = paste("answer", 1:100)), judge,
    input_cols = "input", n_runs = 5, label_levels = c("P", "C"),
    n_boot = 200, seed = 1, audit_id = "gate"
  )
  expect_identical(
    st$pairwise_disagreement_worst_case_high,
    st$pairwise_disagreement_conf_high
  )
  rows <- gate_rows(st, gate_policy())
  expect_all(vapply(rows, `[[`, "", "status") == "PASS")
  expect_false(grepl("missing", rows$stability_pairwise_disagreement$reason))
})

test_that("abstaining on the disagreeing run cannot beat the bootstrap bound", {
  # 96 items agree and 4 disagree over two runs; dropping the 4 disagreeing
  # second-run judgments must not tighten the assessed bound below what the
  # complete evidence gives.
  complete <- cbind(rep("A", 100), c(rep("A", 96), rep("B", 4)))
  abstained <- complete
  abstained[97:100, 2] <- NA
  summarize <- function(runs) {
    withr::with_seed(86, yourhonor:::summarize_stability_runs(
      runs, n_boot = 30, audit_id = "gate"
    ))
  }
  full <- summarize(complete)
  abst <- summarize(abstained)
  expect_gte(
    abst$pairwise_disagreement_worst_case_high,
    full$pairwise_disagreement_conf_high
  )
  # A threshold between the abstaining sample's observed upper and the
  # complete sample's: the complete evidence is INDETERMINATE there, and the
  # abstaining evidence must not do better.
  threshold <- 0.095
  expect_lt(abst$pairwise_disagreement_conf_high, threshold)
  expect_gt(full$pairwise_disagreement_conf_high, threshold)
  pol <- gate_policy(min_n = 96, min_runs = 2, max_disagreement = threshold)
  status <- function(st) gate_rows(st, pol)$stability_pairwise_disagreement$status
  expect_identical(status(full), "INDETERMINATE")
  expect_identical(status(abst), "INDETERMINATE")
})

test_that("a missing or malformed worst-case bound is INDETERMINATE, never an error", {
  st <- deviating_retest(abstain = FALSE)
  pol <- gate_policy()
  # Evidence whose observed interval clears the threshold, so a fallback to
  # the observed bound would PASS.
  clean <- withr::with_seed(1, yourhonor:::summarize_stability_runs(
    matrix(rep(c("A", "B"), each = 100), 200, 3), n_boot = 200,
    audit_id = "gate"
  ))
  expect_identical(
    stored_rows(clean, pol)$stability_pairwise_disagreement$status, "PASS"
  )
  for (value in list(NULL, NA_real_, NaN, Inf, numeric(0), c(0.01, 0.02), "0")) {
    bad_clean <- clean
    bad_clean["pairwise_disagreement_worst_case_high"] <- list(value)
    expect_identical(
      stored_rows(bad_clean, pol)$stability_pairwise_disagreement$status,
      "INDETERMINATE"
    )
    bad <- st
    bad["pairwise_disagreement_worst_case_high"] <- list(value)
    row <- stored_rows(bad, pol)$stability_pairwise_disagreement
    expect_identical(row$status, "INDETERMINATE")
  }
  # A bound below the observed upper, but inside the observed interval and
  # under the threshold, cannot clear the check.
  low <- st
  low$pairwise_disagreement_worst_case_high <- 0.08
  expect_lt(st$pairwise_disagreement_conf_low, 0.08)
  expect_gt(st$pairwise_disagreement_conf_high, 0.1)
  row <- stored_rows(low, pol)$stability_pairwise_disagreement
  expect_identical(row$status, "INDETERMINATE")
  expect_equal(row$value, st$pairwise_disagreement_conf_high)
})

test_that("the interval check alone keeps an observed FAIL without a worst-case bound", {
  runs <- cbind(rep("A", 100), c(rep("A", 60), rep("B", 40)))
  st <- withr::with_seed(1, yourhonor:::summarize_stability_runs(
    runs, n_boot = 200, audit_id = "gate"
  ))
  pol <- gate_policy(min_n = 50, min_runs = 2)
  expect_identical(
    stored_rows(st, pol)$stability_pairwise_disagreement$status, "FAIL"
  )
  st$pairwise_disagreement_worst_case_high <- NULL
  expect_identical(
    stored_rows(st, pol)$stability_pairwise_disagreement$status, "FAIL"
  )
})

test_that("a FAIL with missing judgments holds under their best case", {
  runs <- cbind(rep("A", 100), c(rep("A", 60), rep("B", 38), NA, NA))
  st <- withr::with_seed(1, yourhonor:::summarize_stability_runs(
    runs, n_boot = 200, audit_id = "gate"
  ))
  row <- gate_rows(st, gate_policy(min_n = 50, min_runs = 2))
  disagreement <- row$stability_pairwise_disagreement
  expect_identical(disagreement$status, "FAIL")
  # The two incomplete items count as agreeing in the lower bound, which
  # still exceeds the threshold, and the value is the assessed upper.
  expect_equal(st$pairwise_disagreement_best_case, 0.38)
  expect_gt(st$pairwise_disagreement_best_case_low, 0.1)
  expect_lt(
    st$pairwise_disagreement_best_case_low, st$pairwise_disagreement_conf_low
  )
  expect_identical(disagreement$value, st$pairwise_disagreement_worst_case_high)
  expect_match(disagreement$reason, "The lower bound gives each of the 2 missing judgments")
  expect_match(disagreement$reason, "The upper bound counts every pair")
})

# 100 items whose three runs read A, A, B: complete, the interval clears 0.85.
aab_runs <- function() matrix(rep(c("A", "A", "B"), each = 100), 100, 3)

summarize_gate <- function(runs, n_boot = 500) {
  withr::with_seed(1, yourhonor:::summarize_stability_runs(
    runs, n_boot = n_boot, audit_id = "gate"
  ))
}

test_that("abstaining on an agreeing run cannot FAIL a judge that would PASS", {
  complete <- aab_runs()
  abstained <- complete
  abstained[, 1] <- NA
  pol <- gate_policy(max_missing = 0.5, min_n = 50, min_runs = 3,
                     max_disagreement = 0.85)
  full <- gate_rows(summarize_gate(complete), pol)
  expect_identical(full$stability_pairwise_disagreement$status, "PASS")
  st <- summarize_gate(abstained)
  # Observed, the two runs left disagree on every item: [0.963, 1].
  expect_gt(st$pairwise_disagreement_conf_low, 0.85)
  # Each missing judgment's best label is A or B, one agreeing pair in three.
  expect_equal(st$pairwise_disagreement_best_case, 2 / 3)
  expect_equal(st$per_item$pairwise_disagreement_best_case, rep(2 / 3, 100))
  rows <- gate_rows(st, pol)
  expect_identical(rows$stability_pairwise_disagreement$status, "INDETERMINATE")
  expect_match(
    rows$stability_pairwise_disagreement$reason,
    "The lower bound gives each of the 100 missing judgments", fixed = TRUE
  )
  expect_none(vapply(rows, `[[`, "", "status") == "FAIL")
})

test_that("a judge whose every completion disagrees still FAILs", {
  # Runs B and C disagree, and no label for the missing run agrees with
  # both: every completion has disagreement 2/3. A bound that counted every
  # missing pair as agreeing (1/3) would let this judge dodge the FAIL.
  runs <- matrix(rep(c(NA, "B", "C"), each = 100), 100, 3)
  st <- summarize_gate(runs)
  expect_equal(st$pairwise_disagreement_best_case, 2 / 3)
  pol <- gate_policy(max_missing = 0.5, min_n = 50, min_runs = 3,
                     max_disagreement = 0.5)
  rows <- gate_rows(st, pol)
  expect_identical(rows$stability_pairwise_disagreement$status, "FAIL")
  # The same judge with every judgment present fails too.
  complete <- runs
  complete[, 1] <- "A"
  expect_identical(
    gate_rows(summarize_gate(complete), pol)$stability_pairwise_disagreement$status,
    "FAIL"
  )
})

test_that("abstentions inside a missing-rate tolerance cannot cause a FAIL", {
  # Five runs; 10% of items change label in run 5. On some of those items
  # the judge abstains in three of the four agreeing runs, so the observed
  # disagreement doubles; the complete evidence does not FAIL.
  cases <- list(
    list(n = 200, flips = 20, holes = 16, threshold = 0.05, n_boot = 500,
         complete = "INDETERMINATE"),
    list(n = 5000, flips = 200, holes = 200, threshold = 0.03, n_boot = 200,
         complete = "PASS")
  )
  for (case in cases) {
    truth <- matrix("A", case$n, 5)
    truth[seq_len(case$flips), 5] <- "B"
    observed <- truth
    observed[seq_len(case$holes), 1:3] <- NA
    pol <- gate_policy(max_missing = 0.05, min_n = 50, min_runs = 5,
                       max_disagreement = case$threshold)
    full <- summarize_gate(truth, case$n_boot)
    expect_identical(
      gate_rows(full, pol)$stability_pairwise_disagreement$status,
      case$complete
    )
    st <- summarize_gate(observed, case$n_boot)
    expect_lte(st$missing_rate, 0.05)
    # The observed lower bound alone would FAIL.
    expect_gt(st$pairwise_disagreement_conf_low, case$threshold)
    # The modal fill restores the complete evidence exactly.
    expect_identical(
      st$per_item$pairwise_disagreement_best_case,
      full$per_item$pairwise_disagreement
    )
    rows <- gate_rows(st, pol)
    statuses <- vapply(rows, `[[`, "", "status")
    expect_identical(
      rows$stability_pairwise_disagreement$status, "INDETERMINATE"
    )
    expect_identical(rows$stability_missing_rate$status, "PASS")
    expect_none(statuses == "FAIL")
  }
})

test_that("with nothing missing the best case is the observed interval", {
  withr::local_seed(5)
  runs <- matrix(sample(c("A", "B", "C"), 60 * 4, TRUE), 60, 4)
  st <- summarize_gate(runs, 200)
  expect_identical(
    st$per_item$pairwise_disagreement_best_case,
    st$per_item$pairwise_disagreement
  )
  expect_identical(st$pairwise_disagreement_best_case, st$pairwise_disagreement)
  expect_identical(
    st$pairwise_disagreement_best_case_low, st$pairwise_disagreement_conf_low
  )
  # No extra random numbers: the next draw is the one it was before.
  after <- withr::with_seed(1, {
    yourhonor:::summarize_stability_runs(runs, n_boot = 200)
    stats::runif(1)
  })
  expect_identical(after, withr::with_seed(1, {
    for (i in seq_len(200)) sample.int(60L, 60L, replace = TRUE)
    stats::runif(1)
  }))
})

test_that("the best case is below every completion's interval on the same draws", {
  withr::local_seed(21)
  runs <- matrix(sample(c("A", "B", "C"), 40 * 3, TRUE, prob = c(6, 2, 1)), 40, 3)
  runs[sample(length(runs), 30)] <- NA
  runs[1, ] <- NA
  runs[2, 1:2] <- NA
  st <- summarize_gate(runs, 200)
  holes <- which(is.na(runs))
  for (i in 1:25) {
    completed <- runs
    completed[holes] <- sample(c("A", "B", "C", "D"), length(holes), TRUE)
    full <- summarize_gate(completed, 200)
    expect_true(all(
      st$per_item$pairwise_disagreement_best_case <=
        full$per_item$pairwise_disagreement
    ))
    expect_lte(
      st$pairwise_disagreement_best_case_low,
      full$pairwise_disagreement_conf_low
    )
  }
  # The modal fill is the per-item minimum over every completion, unseen
  # labels included.
  for (z in list(c("A", NA, "B"), c("A", "A", NA, "B", NA), c(NA, NA, "C"),
                 c(NA, NA, NA), c("A", "B", "C", NA))) {
    k <- sum(is.na(z))
    fills <- as.matrix(expand.grid(
      rep(list(c("A", "B", "C", "D")), k), stringsAsFactors = FALSE
    ))
    values <- apply(fills, 1, function(f) {
      y <- z
      y[is.na(y)] <- f
      yourhonor:::item_pairwise_disagreement(matrix(y, 1))
    })
    expect_identical(
      yourhonor:::item_best_case_disagreement(matrix(z, 1)), min(values)
    )
  }
})

test_that("the interval check alone: a malformed best-case bound blocks a FAIL only when judgments are missing", {
  pol <- gate_policy(max_missing = 0.5, min_n = 50, min_runs = 2)
  holed <- cbind(rep("A", 100), c(rep("A", 60), rep("B", 38), NA, NA))
  complete <- cbind(rep("A", 100), c(rep("A", 60), rep("B", 40)))
  st_holed <- summarize_gate(holed, 200)
  st_complete <- summarize_gate(complete, 200)
  status <- function(st) stored_rows(st, pol)$stability_pairwise_disagreement$status
  expect_identical(status(st_holed), "FAIL")
  expect_identical(status(st_complete), "FAIL")
  for (value in list(NULL, NA_real_, NaN, Inf, -0.1, 2, numeric(0),
                     c(0.3, 0.4), "0.3", TRUE)) {
    bad <- st_holed
    bad["pairwise_disagreement_best_case_low"] <- list(value)
    row <- stored_rows(bad, pol)$stability_pairwise_disagreement
    expect_identical(row$status, "INDETERMINATE")
    expect_match(row$reason, "best-case stability bound", fixed = TRUE)
    # With nothing missing the observed lower bound is the best case.
    bad <- st_complete
    bad["pairwise_disagreement_best_case_low"] <- list(value)
    expect_identical(status(bad), "FAIL")
  }
  # Without the run matrix, or with one that is not the declared evidence,
  # nothing shows that no judgment is missing.
  for (runs in list(NULL, complete[1:50, ], as.vector(complete))) {
    bad <- st_complete
    bad$pairwise_disagreement_best_case_low <- NULL
    bad["runs"] <- list(runs)
    expect_identical(status(bad), "INDETERMINATE")
  }
  # Nor can a malformed best case repair a PASS it blocks: a bound that
  # does not exist still lets a clean upper bound PASS.
  clean <- summarize_gate(cbind(rep("A", 100), c(rep("A", 99), NA)), 200)
  clean$pairwise_disagreement_best_case_low <- NA_real_
  expect_identical(status(clean), "PASS")
})

test_that("the interval check alone: with nothing missing a stored best case cannot move the lower bound", {
  pol <- gate_policy(max_missing = 0.5, min_n = 50, min_runs = 2)
  st <- summarize_gate(cbind(rep("A", 100), c(rep("A", 60), rep("B", 40))), 200)
  for (value in list(0, 0.05)) {
    bad <- st
    bad$pairwise_disagreement_best_case_low <- value
    row <- stored_rows(bad, pol)$stability_pairwise_disagreement
    expect_identical(row$status, "FAIL")
  }
})

test_that("an observed interval outside [0, 1] is unavailable, not assessed", {
  pol <- gate_policy(max_missing = 0.5, min_n = 50, min_runs = 2,
                     max_disagreement = 0.2)
  st <- summarize_gate(cbind(rep("A", 100), rep("A", 100)), 200)
  for (bounds in list(c(-0.1, 0.1), c(0.3, 1.5))) {
    bad <- st
    bad$pairwise_disagreement_conf_low <- bounds[[1]]
    bad$pairwise_disagreement_conf_high <- bounds[[2]]
    bad$pairwise_disagreement_best_case_low <- 0
    bad$pairwise_disagreement_worst_case_high <- 0.1
    row <- stored_rows(bad, pol)$stability_pairwise_disagreement
    expect_identical(row$status, "INDETERMINATE")
    expect_identical(row$reason, "Interval is unavailable.")
  }
})

test_that("a worst-case bound that is not a probability cannot PASS", {
  pol <- gate_policy(max_missing = 0.5, min_n = 50, min_runs = 2)
  st <- summarize_gate(cbind(rep("A", 100), c(rep("A", 95), NA, NA, NA, NA, NA)), 200)
  expect_identical(
    stored_rows(st, pol)$stability_pairwise_disagreement$status, "INDETERMINATE"
  )
  expect_lt(st$pairwise_disagreement_conf_high, 0.1)
  for (value in list(-1, 1.5)) {
    bad <- st
    bad$pairwise_disagreement_worst_case_high <- value
    expect_identical(
      stored_rows(bad, pol)$stability_pairwise_disagreement$status,
      "INDETERMINATE"
    )
  }
})

test_that("an unavailable count or missing rate is INDETERMINATE, never FAIL or an error", {
  st <- summarize_gate(cbind(rep("A", 100), rep("A", 100)), 200)
  pol <- gate_policy(min_n = 50, min_runs = 2)
  for (value in list(NA_integer_, NA_real_, NaN, NULL, c(1, 2), "100")) {
    bad <- st
    bad["n_items_scored"] <- list(value)
    row <- stored_rows(bad, pol)$stability_items
    expect_identical(row$status, "INDETERMINATE")
    expect_identical(row$reason, "Sample size is unavailable.")
    bad <- st
    bad["missing_rate"] <- list(value)
    row <- stored_rows(bad, pol)$stability_missing_rate
    expect_identical(row$status, "INDETERMINATE")
    expect_identical(row$reason, "Missing rate is unavailable.")
    expect_true(is.na(row$value))
  }
})

test_that("a NaN judgment is missing, not a perfectly stable label", {
  ids <- sprintf("i%03d", 1:100)
  rows <- expand.grid(id = ids, epoch = 1:5, stringsAsFactors = FALSE)
  rows$input <- paste("q", rows$id)
  rows$score <- ifelse(match(rows$id, ids) <= 20, NaN, 1)
  st <- stability_vitals(
    rows, content_cols = "input", n_boot = 200, seed = 1, audit_id = "gate"
  )
  expect_equal(st$missing_rate, 0.2)
  expect_false("NaN" %in% st$label_levels)
  pol <- gate_policy(max_missing = 0.2, min_n = 50, min_runs = 3,
                     max_disagreement = 0.05)
  expect_false(identical(
    gate_rows(st, pol)$stability_pairwise_disagreement$status, "PASS"
  ))

  judge <- function(d) {
    item <- as.integer(sub("answer ", "", d$input, fixed = TRUE))
    ifelse(item <= 20, NaN, 1)
  }
  retest <- test_retest(
    tibble::tibble(input = paste("answer", 1:100)), judge,
    input_cols = "input", n_runs = 3, n_boot = 200, seed = 1,
    audit_id = "gate"
  )
  expect_equal(retest$missing_rate, 0.2)
  expect_false("NaN" %in% retest$label_levels)
  expect_false(anyNA(retest$runs[21:100, ]))
  expect_all(is.na(retest$runs[1:20, ]))
  complex_judge <- function(d) {
    item <- as.integer(sub("answer ", "", d$input, fixed = TRUE))
    ifelse(item <= 20, complex(real = NaN), 1 + 0i)
  }
  complex_retest <- test_retest(
    tibble::tibble(input = paste("answer", 1:100)), complex_judge,
    input_cols = "input", n_runs = 3, n_boot = 50, seed = 1
  )
  expect_equal(complex_retest$missing_rate, 0.2)
  # A list score column: list(NA), NULL and NaN cells are missing, not the
  # labels "NA", "NULL" or "NaN"; one scalar per cell is still accepted.
  listed <- rows
  listed$score <- as.list(ifelse(is.nan(listed$score), 1, listed$score))
  unstable <- which(match(listed$id, ids) <= 20)
  third <- seq_along(unstable) %% 3L
  listed$score[unstable[third == 1L]] <- list(NA)
  listed$score[unstable[third == 2L]] <- list(NULL)
  listed$score[unstable[third == 0L]] <- list(NaN)
  from_list <- stability_vitals(
    listed, content_cols = "input", n_boot = 20, seed = 1
  )
  expect_equal(from_list$missing_rate, 0.2)
  expect_identical(from_list$label_levels, "1")
  clean_list <- listed
  clean_list$score <- as.list(rep("1", nrow(listed)))
  expect_equal(
    stability_vitals(clean_list, content_cols = "input", n_boot = 20,
                     seed = 1)$missing_rate,
    0
  )
  several <- clean_list
  several$score[[1]] <- c("1", "2")
  expect_error(
    stability_vitals(several, content_cols = "input", n_boot = 20, seed = 1),
    class = "yourhonor_input_error"
  )
  matrixed <- clean_list
  matrixed$score <- matrix("1", nrow(listed), 2)
  expect_error(
    stability_vitals(matrixed, content_cols = "input", n_boot = 20, seed = 1),
    class = "yourhonor_input_error"
  )
})

test_that("print shows the worst case only when every worst-case field exists", {
  st <- withr::with_seed(1, yourhonor:::summarize_stability_runs(
    cbind(rep("A", 100), c(rep("A", 98), NA, NA)), n_boot = 20
  ))
  printed <- function(x) paste(cli::cli_fmt(print(x)), collapse = " ")
  expect_match(printed(st), "Worst case", fixed = TRUE)
  for (field in c("pairwise_disagreement_worst_case",
                  "pairwise_disagreement_worst_case_high",
                  "any_flip_rate_worst_case")) {
    partial <- st
    partial[[field]] <- NULL
    expect_false(grepl("Worst case", printed(partial), fixed = TRUE))
  }
  padded <- st
  padded$any_flip_rate_worst_case <- NULL
  padded$pairwise_disagreement_worst_case_high <- c(0.02, 0.03)
  expect_false(grepl("Worst case", printed(padded), fixed = TRUE))
})

test_that("a malformed observed interval is INDETERMINATE, never an error", {
  st <- deviating_retest(abstain = FALSE)
  pol <- gate_policy()
  for (field in c("pairwise_disagreement_conf_low",
                  "pairwise_disagreement_conf_high")) {
    for (value in list(NULL, c(0.01, 0.02), "0.01")) {
      bad <- st
      bad[field] <- list(value)
      row <- stored_rows(bad, pol)$stability_pairwise_disagreement
      expect_identical(row$status, "INDETERMINATE")
    }
  }
})

test_that("a reversed observed interval is not repaired by the worst-case bound", {
  st <- deviating_retest(abstain = FALSE)
  pol <- gate_policy()
  reversed <- st
  reversed$pairwise_disagreement_conf_low <- 0.20
  reversed$pairwise_disagreement_conf_high <- 0.15
  reversed$pairwise_disagreement_worst_case_high <- 0.30
  row <- stored_rows(reversed, pol)$stability_pairwise_disagreement
  expect_identical(row$status, "INDETERMINATE")
  expect_match(row$reason, "reversed")
  narrow <- st
  narrow$pairwise_disagreement_conf_low <- 0.05
  narrow$pairwise_disagreement_conf_high <- 0.04
  narrow$pairwise_disagreement_worst_case_high <- 0.08
  row <- stored_rows(narrow, pol)$stability_pairwise_disagreement
  expect_identical(row$status, "INDETERMINATE")
})

test_that("holes in otherwise passing evidence never produce a FAIL", {
  # Same evidence with and without abstentions: the observed lower bound is
  # unchanged, so a FAIL needs the observed interval to exceed the threshold.
  withr::local_seed(11)
  base <- matrix(sample(c("A", "B"), 200 * 4, TRUE, prob = c(0.97, 0.03)), 200, 4)
  base[, 2:4] <- base[, 1]
  base[1:6, 4] <- ifelse(base[1:6, 1] == "A", "B", "A")
  holed <- base
  holed[sample(length(holed), 40)] <- NA
  pol <- gate_policy(max_missing = 0.2, min_n = 50, min_runs = 2)
  for (runs in list(base, holed)) {
    st <- withr::with_seed(1, yourhonor:::summarize_stability_runs(
      runs, n_boot = 200, audit_id = "gate"
    ))
    statuses <- vapply(gate_rows(st, pol), `[[`, "", "status")
    expect_none(statuses == "FAIL")
  }
})

test_that("a stored worst case below the observed upper cannot PASS a holed interval", {
  pol <- gate_policy(max_missing = 0.5, min_n = 50, min_runs = 2)
  runs <- matrix("A", 100, 3)
  runs[1:40, 3] <- NA
  st <- summarize_gate(runs, 200)
  expect_gte(st$pairwise_disagreement_worst_case_high,
             st$pairwise_disagreement_conf_high)
  status <- function(x) stored_rows(x, pol)$stability_pairwise_disagreement$status
  expect_identical(status(st), "INDETERMINATE")
  for (value in list(0, st$pairwise_disagreement_conf_high / 2)) {
    bad <- st
    bad$pairwise_disagreement_worst_case_high <- value
    row <- stored_rows(bad, pol)$stability_pairwise_disagreement
    expect_identical(row$status, "INDETERMINATE")
    expect_match(row$reason, "worst-case stability bound", fixed = TRUE)
  }
  # With nothing missing the observed upper bound is the worst case.
  complete <- summarize_gate(matrix("A", 100, 3), 200)
  expect_identical(status(complete), "PASS")
  complete$pairwise_disagreement_worst_case_high <- 0
  expect_identical(status(complete), "PASS")
})

test_that("a stored best case above the observed lower bound cannot hold a holed FAIL", {
  pol <- gate_policy(max_missing = 1, min_n = 50, min_runs = 2,
                     max_disagreement = 0.75)
  runs <- aab_runs()
  runs[1:60, 1] <- NA
  st <- summarize_gate(runs, 300)
  expect_lte(st$pairwise_disagreement_best_case_low,
             st$pairwise_disagreement_conf_low)
  expect_gt(st$pairwise_disagreement_conf_low, 0.75)
  status <- function(x) stored_rows(x, pol)$stability_pairwise_disagreement$status
  expect_identical(status(st), "INDETERMINATE")
  for (value in list(1, (st$pairwise_disagreement_conf_low + 1) / 2)) {
    bad <- st
    bad$pairwise_disagreement_best_case_low <- value
    row <- stored_rows(bad, pol)$stability_pairwise_disagreement
    expect_identical(row$status, "INDETERMINATE")
    expect_match(row$reason, "best-case stability bound", fixed = TRUE)
  }
})

# Hand-edited stability evidence: the policy rebuilds every bound and count
# it reads from the object's runs and bootstrap state, so an edit can
# neither PASS nor FAIL a judge.
statuses <- function(rows) vapply(rows, `[[`, "", "status")
mismatch_reason <- "Stored stability bounds or counts do not match their rebuild from its runs."

# 300 items over three runs: 10 disagree once and 25 miss a judgment. The
# observed upper clears 0.1; the worst case does not.
holed_retest <- function() {
  runs <- matrix("A", 300, 3)
  runs[1:10, 3] <- "B"
  runs[11:35, 3] <- NA
  summarize_gate(runs, 200)
}

test_that("an edited worst case cannot PASS holed evidence", {
  pol <- gate_policy(max_missing = 0.05, min_n = 50, min_runs = 2)
  st <- holed_retest()
  expect_lt(st$pairwise_disagreement_conf_high, 0.1)
  expect_gt(st$pairwise_disagreement_worst_case_high, 0.1)
  expect_identical(
    gate_rows(st, pol)$stability_pairwise_disagreement$status, "INDETERMINATE"
  )
  # The worst case set to the observed upper, which a genuine bound may
  # equal: before, the policy trusted it and passed.
  bad <- st
  bad$pairwise_disagreement_worst_case_high <- st$pairwise_disagreement_conf_high
  rows <- gate_rows(bad, pol)
  expect_identical(unname(statuses(rows)), rep("INDETERMINATE", 4))
  expect_identical(
    unname(vapply(rows, `[[`, "", "reason")), rep(mismatch_reason, 4)
  )
  expect_true(all(is.na(vapply(rows, `[[`, 0, "value"))))
  expect_identical(
    names(rows),
    c("stability_pairwise_disagreement", "stability_items", "stability_runs",
      "stability_missing_rate")
  )
  expect_identical(
    vapply(rows, `[[`, 0, "threshold"),
    vapply(gate_rows(st, pol), `[[`, 0, "threshold")
  )
})

test_that("edited observed bounds cannot PASS complete evidence", {
  pol <- gate_policy(max_missing = 0, min_n = 50, min_runs = 2)
  runs <- matrix("A", 100, 3)
  runs[1:40, 3] <- "B"
  st <- summarize_gate(runs, 200)
  expect_identical(
    gate_rows(st, pol)$stability_pairwise_disagreement$status, "FAIL"
  )
  bad <- st
  bad$pairwise_disagreement_conf_low <- 0.01
  bad$pairwise_disagreement_conf_high <- 0.05
  bad$pairwise_disagreement_worst_case_high <- 0.05
  bad$pairwise_disagreement_best_case_low <- 0.01
  expect_identical(unname(statuses(gate_rows(bad, pol))), rep("INDETERMINATE", 4))
})

test_that("an edited bound or count cannot FAIL a judge that passes", {
  pol <- gate_policy(max_missing = 0, min_n = 50, min_runs = 2)
  st <- summarize_gate(matrix("A", 100, 3), 200)
  expect_identical(unname(statuses(gate_rows(st, pol))), rep("PASS", 4))
  bad <- st
  bad$pairwise_disagreement_conf_low <- 0.5
  bad$pairwise_disagreement_conf_high <- 0.6
  bad$pairwise_disagreement_worst_case_high <- 0.6
  bad$pairwise_disagreement_best_case_low <- 0.5
  expect_identical(unname(statuses(gate_rows(bad, pol))), rep("INDETERMINATE", 4))
  bad <- st
  bad$missing_rate <- 0.5
  expect_identical(unname(statuses(gate_rows(bad, pol))), rep("INDETERMINATE", 4))
})

test_that("an edited missing rate or count cannot clear its check", {
  pol <- gate_policy(max_missing = 0.01, min_n = 50, min_runs = 2)
  st <- holed_retest()
  rows <- gate_rows(st, pol)
  expect_identical(rows$stability_missing_rate$status, "FAIL")
  bad <- st
  bad$missing_rate <- 0
  expect_identical(unname(statuses(gate_rows(bad, pol))), rep("INDETERMINATE", 4))
  small <- summarize_gate(matrix(c(rep("A", 40), rep("A", 40)), 40, 2), 200)
  expect_identical(
    gate_rows(small, pol)$stability_items$status, "INDETERMINATE"
  )
  bad <- small
  bad$n_items_scored <- 60L
  rows <- gate_rows(bad, pol)
  expect_identical(rows$stability_items$reason, mismatch_reason)
})

test_that("stability evidence that cannot be rebuilt is INDETERMINATE", {
  pol <- gate_policy(max_missing = 0, min_n = 50, min_runs = 2)
  st <- summarize_gate(matrix("A", 100, 3), 200)
  for (field in c("bootstrap_seed", "runs")) {
    old <- st
    old[[field]] <- NULL
    rows <- gate_rows(old, pol)
    expect_identical(unname(statuses(rows)), rep("INDETERMINATE", 4))
    expect_identical(
      rows$stability_runs$reason,
      "Stability evidence cannot be rebuilt from its runs."
    )
  }
})

test_that("genuine stability evidence gets the checks it got before", {
  # The rows with the rebuild are the rows the stored values give: the
  # rebuild is bit for bit the stored evidence.
  cal <- gate_calibration("gate")
  cases <- list(
    holed_retest(), summarize_gate(matrix("A", 100, 3), 200),
    summarize_gate(aab_runs(), 300), deviating_retest(TRUE),
    deviating_retest(FALSE), hidden_vitals(TRUE), hidden_vitals(FALSE)
  )
  for (threshold in c(0.05, 0.1, 0.4, 0.85)) {
    pol <- gate_policy(max_missing = 0.2, min_n = 50, min_runs = 2,
                       max_disagreement = threshold)
    with_rebuild <- lapply(cases, function(st) stability_checks(cal, st, pol))
    stored <- local({
      local_mocked_bindings(
        verify_stability_evidence = function(stability) {
          list(stability = stability, reason = NULL)
        }
      )
      lapply(cases, function(st) stability_checks(cal, st, pol))
    })
    expect_identical(with_rebuild, stored)
  }
  # A genuine bad judge with complete evidence still FAILs.
  pol <- gate_policy(max_missing = 0.2, min_n = 50, min_runs = 2,
                     max_disagreement = 0.05)
  expect_identical(
    gate_rows(deviating_retest(FALSE), pol)$stability_pairwise_disagreement$status,
    "FAIL"
  )
})

test_that("report() and write_audit() do not pass edited stability evidence", {
  cal <- make_cal()
  pol <- make_policy(max_pairwise_disagreement = 0.1)
  st <- make_stability()
  expect_identical(report(cal, stability = st, policy = pol)$decision, "PASS")
  bad <- st
  bad$pairwise_disagreement_conf_low <- 0.5
  bad$pairwise_disagreement_conf_high <- 0.6
  bad$pairwise_disagreement_worst_case_high <- 0.6
  bad$pairwise_disagreement_best_case_low <- 0.5
  rep <- report(cal, stability = bad, policy = pol)
  expect_identical(rep$decision, "INDETERMINATE")
  stab <- rep$checks[startsWith(rep$checks$check, "stability"), ]
  expect_identical(stab$status, rep("INDETERMINATE", 4))
  expect_identical(unique(stab$reason), mismatch_reason)
  path <- withr::local_tempfile(fileext = ".json")
  write_audit(cal, path, stability = bad, policy = pol)
  json <- jsonlite::read_json(path)
  checks <- Filter(function(row) startsWith(row$check, "stability"), json$checks)
  expect_length(checks, 4)
  for (row in checks) {
    expect_identical(row$status, "INDETERMINATE")
    expect_null(row$value)
  }
  # The JSON writes neither the run matrix nor the state.
  expect_false(any(c("runs", "bootstrap_seed") %in% names(json$stability)))
})
