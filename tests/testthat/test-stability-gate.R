# Policy-level tests of the stability gate under missing judgments: an
# abstention may widen the assessed upper bound (PASS -> INDETERMINATE) but
# can never clear it, and can never turn a check into FAIL.

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
  expect_true(all(vapply(rows, `[[`, "", "status") == "PASS"))
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
    gate_rows(clean, pol)$stability_pairwise_disagreement$status, "PASS"
  )
  for (value in list(NULL, NA_real_, NaN, Inf, numeric(0), c(0.01, 0.02), "0")) {
    bad_clean <- clean
    bad_clean["pairwise_disagreement_worst_case_high"] <- list(value)
    expect_identical(
      gate_rows(bad_clean, pol)$stability_pairwise_disagreement$status,
      "INDETERMINATE"
    )
    bad <- st
    bad["pairwise_disagreement_worst_case_high"] <- list(value)
    row <- gate_rows(bad, pol)$stability_pairwise_disagreement
    expect_identical(row$status, "INDETERMINATE")
  }
  # A bound below the observed upper, but inside the observed interval and
  # under the threshold, cannot clear the check.
  low <- st
  low$pairwise_disagreement_worst_case_high <- 0.08
  expect_lt(st$pairwise_disagreement_conf_low, 0.08)
  expect_gt(st$pairwise_disagreement_conf_high, 0.1)
  row <- gate_rows(low, pol)$stability_pairwise_disagreement
  expect_identical(row$status, "INDETERMINATE")
  expect_equal(row$value, st$pairwise_disagreement_conf_high)
})

test_that("without a worst-case bound an observed FAIL stays FAIL", {
  runs <- cbind(rep("A", 100), c(rep("A", 60), rep("B", 40)))
  st <- withr::with_seed(1, yourhonor:::summarize_stability_runs(
    runs, n_boot = 200, audit_id = "gate"
  ))
  pol <- gate_policy(min_n = 50, min_runs = 2)
  expect_identical(
    gate_rows(st, pol)$stability_pairwise_disagreement$status, "FAIL"
  )
  st$pairwise_disagreement_worst_case_high <- NULL
  expect_identical(
    gate_rows(st, pol)$stability_pairwise_disagreement$status, "FAIL"
  )
})

test_that("a FAIL with missing judgments is not attributed to them", {
  runs <- cbind(rep("A", 100), c(rep("A", 60), rep("B", 38), NA, NA))
  st <- withr::with_seed(1, yourhonor:::summarize_stability_runs(
    runs, n_boot = 200, audit_id = "gate"
  ))
  row <- gate_rows(st, gate_policy(min_n = 50, min_runs = 2))
  expect_identical(row$stability_pairwise_disagreement$status, "FAIL")
  expect_false(grepl("missing", row$stability_pairwise_disagreement$reason))
  expect_equal(
    row$stability_pairwise_disagreement$value,
    st$pairwise_disagreement_conf_high
  )
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
  expect_true(all(is.na(retest$runs[1:20, ])))
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
  listed$score[unstable[c(TRUE, FALSE, FALSE)]] <- list(NA)
  listed$score[unstable[c(FALSE, TRUE, FALSE)]] <- list(NULL)
  listed$score[unstable[c(FALSE, FALSE, TRUE)]] <- list(NaN)
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
    "one score or nothing"
  )
  matrixed <- clean_list
  matrixed$score <- matrix("1", nrow(listed), 2)
  expect_error(
    stability_vitals(matrixed, content_cols = "input", n_boot = 20, seed = 1),
    "atomic vector"
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
      row <- gate_rows(bad, pol)$stability_pairwise_disagreement
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
  row <- gate_rows(reversed, pol)$stability_pairwise_disagreement
  expect_identical(row$status, "INDETERMINATE")
  expect_match(row$reason, "reversed")
  narrow <- st
  narrow$pairwise_disagreement_conf_low <- 0.05
  narrow$pairwise_disagreement_conf_high <- 0.04
  narrow$pairwise_disagreement_worst_case_high <- 0.08
  row <- gate_rows(narrow, pol)$stability_pairwise_disagreement
  expect_identical(row$status, "INDETERMINATE")
})

test_that("holes in otherwise passing evidence never produce a FAIL", {
  # Same evidence with and without abstentions: the observed lower bound is
  # unchanged, so a FAIL needs the observed interval to exceed the threshold.
  set.seed(11)
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
    expect_false(any(statuses == "FAIL"))
  }
})
