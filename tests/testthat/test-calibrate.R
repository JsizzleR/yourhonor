make_gold <- function() {
  tibble::tibble(
    id = paste0("q", 1:8),
    input = paste("answer", 1:8),
    target = "is it correct?",
    human_label = c("C", "C", "P", "I", "C", "P", "I", "C"),
    judge_label = c("C", "C", "C", "I", "C", "P", "P", "C")
  )
}

test_that("multiple human raters produce reliability and per-rater evidence", {
  gold <- tibble::tibble(
    input = paste("answer", 1:8),
    h1 = c("C", "C", "P", "I", "C", "P", "I", "C"),
    h2 = c("C", "P", "P", "I", "C", "C", "I", "C"),
    h3 = c("C", "C", "P", "P", "C", "P", "I", "C"),
    judge_label = c("C", "C", "C", "I", "C", "P", "P", "C")
  )
  cal <- calibrate_judge(
    gold, human_cols = c("h1", "h2", "h3"),
    label_levels = c("I", "P", "C"), n_boot = 100, seed = 1
  )
  expect_false(is.null(cal$human_reliability))
  expect_identical(cal$human_ceiling, cal$human_reliability)
  expect_setequal(
    cal$human_reliability$coefficient,
    c("krippendorff_alpha", "gwet_ac1")
  )
  expect_setequal(unique(cal$per_rater_agreement$rater), c("h1", "h2", "h3"))
  expect_true(all(c("human_consensus", "consensus_tie") %in% names(cal$items)))
})

test_that("multi-rater consensus requires at least two observed humans", {
  gold <- tibble::tibble(
    h1 = rep(c("I", "C"), 30),
    h2 = NA_character_,
    h3 = NA_character_,
    judge_label = rep(c("I", "C"), 30)
  )
  expect_error(
    calibrate_judge(
      gold, human_cols = c("h1", "h2", "h3"),
      label_levels = c("I", "C"), n_boot = 50, seed = 1
    ),
    "complete judge-human pairs"
  )
})

test_that("single human rater has no reliability benchmark", {
  cal <- calibrate_judge(
    make_gold(), label_levels = c("I", "P", "C"), n_boot = 50, seed = 1
  )
  expect_null(cal$human_reliability)
  expect_null(cal$human_ceiling)
})

test_that("missing human columns and duplicate IDs error", {
  expect_error(
    calibrate_judge(make_gold(), human_cols = c("human_label", "nope")),
    "missing human rater"
  )
  gold <- make_gold()
  gold$id[[2]] <- gold$id[[1]]
  expect_error(calibrate_judge(gold, id_col = "id"), "unique")
})

test_that("pre-graded labels create a complete audit object", {
  cal <- calibrate_judge(
    make_gold(), id_col = "id", label_levels = c("I", "P", "C"),
    n_boot = 100, seed = 1
  )
  expect_s3_class(cal, "yourhonor_calibration")
  expect_equal(cal$n_items, 8)
  expect_equal(cal$n_input, 8)
  expect_identical(cal$label_levels, c("I", "P", "C"))
  expect_true(all(c("support", "recall_conf_low") %in% names(cal$confusion_metrics)))
  expect_equal(cal$provenance$id_col, "id")
})

test_that("judge functions receive only IDs and declared inputs", {
  gold <- make_gold()
  gold$id <- paste0("human-label-", gold$human_label, "-", seq_len(nrow(gold)))
  gold$judge_label <- NULL
  seen <- NULL
  seen_ids <- NULL
  seen_input <- NULL
  judge <- function(d) {
    seen <<- names(d)
    seen_ids <<- d$.item_id
    seen_input <<- d$input
    rep("C", nrow(d))
  }
  cal <- calibrate_judge(
    gold, judge, input_cols = c("input", "target"), id_col = "id",
    label_levels = c("I", "P", "C"), n_boot = 50, seed = 1
  )
  expect_identical(seen, c(".item_id", "input", "target"))
  expect_identical(seen_ids, seq_len(nrow(gold)))
  expect_false(identical(seen_input, gold$input))
  expect_false("human_label" %in% seen)
  expect_false(any(gold$id %in% seen_ids))
  expect_true(all(cal$items$judge_label == "C"))
})

test_that("judge input must be explicit and cannot expose labels", {
  gold <- make_gold()
  gold$judge_label <- NULL
  judge <- function(d) rep("C", nrow(d))
  expect_error(calibrate_judge(gold, judge), "input_cols")
  expect_error(
    calibrate_judge(gold, judge, input_cols = c("input", "human_label")),
    "Protected"
  )
  expect_error(
    calibrate_judge(gold, judge, input_cols = c("input", "input")),
    "unique"
  )
  expect_error(
    calibrate_judge(make_gold(), human_cols = "human_label",
                    judge_col = "human_label"),
    "must differ"
  )
})

test_that("structured judge output is keyed and reordered safely", {
  gold <- make_gold()
  gold$judge_label <- NULL
  judge <- function(d) {
    item <- as.integer(sub("answer ", "", d$input, fixed = TRUE))
    out <- tibble::tibble(
      .item_id = d$.item_id,
      label = ifelse(item %% 2L == 1L, "C", "P")
    )
    out[nrow(out):1, ]
  }
  cal <- calibrate_judge(
    gold, judge, input_cols = "input", id_col = "id",
    label_levels = c("I", "P", "C"), n_boot = 50, seed = 1
  )
  expect_identical(cal$items$judge_label, rep(c("C", "P"), 4))
})

test_that("blind grading order is seeded and does not leak RNG state", {
  gold <- make_gold()
  gold$judge_label <- NULL
  orders <- list()
  judge <- function(d) {
    orders[[length(orders) + 1L]] <<- d$input
    rep("C", nrow(d))
  }
  run <- function() calibrate_judge(
    gold, judge, input_cols = "input", label_levels = c("I", "P", "C"),
    n_boot = 50, seed = 17
  )
  invisible(run())
  first <- orders[[1]]
  invisible(run())
  expect_identical(orders[[2]], first)

  set.seed(123)
  expected <- runif(1)
  set.seed(123)
  invisible(run())
  actual <- runif(1)
  expect_equal(actual, expected)
})

test_that("unknown labels and malformed judge output are rejected", {
  gold <- make_gold()
  gold$judge_label <- NULL
  expect_error(
    calibrate_judge(
      gold, function(d) rep("garbage", nrow(d)), input_cols = "input",
      label_levels = c("I", "P", "C")
    ),
    "outside"
  )
  expect_error(
    calibrate_judge(
      gold, function(d) rep("C", 1), input_cols = "input",
      label_levels = c("I", "P", "C")
    ),
    "return one label per row"
  )
})

test_that("missing judge labels and consensus ties remain visible", {
  gold <- make_gold()
  gold$judge_label[[2]] <- NA
  expect_warning(
    cal <- calibrate_judge(
      gold, label_levels = c("I", "P", "C"), n_boot = 50, seed = 1
    ),
    "lacked"
  )
  expect_equal(cal$n_items, 7)
  expect_equal(cal$n_excluded, 1)

  two <- tibble::tibble(
    h1 = c("C", "C", "P", "P"),
    h2 = c("C", "P", "P", "C"),
    judge_label = c("C", "P", "P", "C")
  )
  expect_warning(
    tied <- calibrate_judge(
      two, human_cols = c("h1", "h2"), label_levels = c("P", "C"),
      n_boot = 50, seed = 1
    ),
    "lacked"
  )
  expect_equal(tied$n_consensus_ties, 2)
  expect_equal(tied$n_excluded, 2)
})

test_that("agreement can reuse or explicitly recompute a calibration", {
  cal <- calibrate_judge(
    make_gold(), label_levels = c("I", "P", "C"), n_boot = 50, seed = 1
  )
  expect_identical(agreement(cal), cal$agreement)
  one <- agreement(cal, coefficients = "cohen_kappa", n_boot = 50, seed = 1)
  expect_equal(one$coefficient, "cohen_kappa")
})

test_that("ordinal calibration requires declared label order", {
  expect_error(calibrate_judge(make_gold(), scale = "ordinal"), "label_levels")
  cal <- calibrate_judge(
    make_gold(), scale = "ordinal", label_levels = c("I", "P", "C"),
    n_boot = 50, seed = 1
  )
  expect_equal(cal$weights, "ordinal")
  expect_true("gwet_ac2" %in% cal$agreement$coefficient)
  expect_false("gwet_ac1" %in% cal$agreement$coefficient)
})
