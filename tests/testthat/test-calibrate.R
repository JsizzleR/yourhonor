make_gold <- function() {
  tibble::tibble(
    input = paste("answer", 1:8),
    target = "is it correct?",
    human_label = c("C", "C", "P", "I", "C", "P", "I", "C"),
    judge_label = c("C", "C", "C", "I", "C", "P", "P", "C")
  )
}

test_that("multiple human raters produce a consensus and a ceiling", {
  gold <- tibble::tibble(
    input = paste("answer", 1:8),
    human_label   = c("C", "C", "P", "I", "C", "P", "I", "C"),
    human_label_2 = c("C", "P", "P", "I", "C", "C", "I", "C"),
    human_label_3 = c("C", "C", "P", "P", "C", "P", "I", "C"),
    judge_label   = c("C", "C", "C", "I", "C", "P", "P", "C")
  )
  cal <- calibrate_judge(
    gold,
    human_cols = c("human_label", "human_label_2", "human_label_3"),
    n_boot = 100, seed = 1
  )
  expect_false(is.null(cal$human_ceiling))
  expect_setequal(cal$human_ceiling$coefficient, c("krippendorff_alpha", "gwet_ac1"))
  expect_true("human_consensus" %in% names(cal$items))
  # item 1 is unanimous C -> consensus C
  expect_equal(cal$items$human_consensus[1], "C")
})

test_that("single human rater has no ceiling (default)", {
  cal <- calibrate_judge(make_gold(), n_boot = 50, seed = 1)
  expect_null(cal$human_ceiling)
})

test_that("missing human columns error clearly", {
  expect_error(
    calibrate_judge(make_gold(), human_cols = c("human_label", "nope")),
    "missing human rater"
  )
})

test_that("calibrate_judge works with pre-graded labels (judge = NULL)", {
  cal <- calibrate_judge(make_gold(), n_boot = 100, seed = 1)
  expect_s3_class(cal, "yourhonor_calibration")
  expect_s3_class(cal$agreement, "tbl_df")
  expect_equal(cal$n_items, 8)
  expect_equal(nrow(cal$items), 8)
  expect_true("agree" %in% names(cal$items))
  expect_equal(cal$seed, 1)
})

test_that("calibrate_judge runs a judge function", {
  gold <- make_gold()
  gold$judge_label <- NULL
  always_c <- function(d) rep("C", nrow(d))
  cal <- calibrate_judge(gold, always_c, n_boot = 50, seed = 1)
  expect_true(all(cal$items$judge_label == "C"))
})

test_that("gold can be piped in", {
  cal <- make_gold() |> calibrate_judge(n_boot = 50, seed = 1)
  expect_s3_class(cal, "yourhonor_calibration")
})

test_that("calibrate_judge errors without human_label", {
  bad <- tibble::tibble(input = "x", judge_label = "C")
  expect_error(calibrate_judge(bad), "human_label")
})

test_that("a judge returning the wrong length errors clearly", {
  gold <- make_gold()
  short_judge <- function(d) "C"
  expect_error(calibrate_judge(gold, short_judge), "one label per row")
})

test_that("missing judge labels are excluded with a warning, not counted", {
  gold <- make_gold()
  gold$judge_label[2] <- NA
  expect_warning(cal <- calibrate_judge(gold, n_boot = 50, seed = 1), "missing label")
  expect_equal(cal$n_items, 7)
  expect_equal(cal$n_excluded, 1)
})

test_that("agreement() accepts a calibration object", {
  cal <- calibrate_judge(make_gold(), n_boot = 50, seed = 1)
  expect_identical(agreement(cal), cal$agreement)
})

test_that("print shows the CI and interpretation, not just the estimate", {
  cal <- calibrate_judge(make_gold(), n_boot = 50, seed = 1)
  out <- cli::cli_fmt(print(cal))
  txt <- paste(out, collapse = "\n")
  expect_match(txt, "judge calibration")
  expect_match(txt, "\\[")           # confidence interval bracket
  expect_match(txt, "moderate|substantial|fair|slight|almost perfect|poor")
})
