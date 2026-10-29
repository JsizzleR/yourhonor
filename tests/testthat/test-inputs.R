# Regression tests for input-handling findings from the whole-codebase review.

test_that("inferred label levels include labels only the judge used", {
  gold <- tibble::tibble(
    human_label = rep("A", 20),
    judge_label = c("B", rep("A", 19))
  )
  cal <- calibrate_judge(gold, n_boot = 20, seed = 1)
  expect_identical(cal$label_levels, c("A", "B"))
  expect_true(all(cal$agreement$status == "insufficient_reference_variation"))

  # Supplied levels are still a contract on the judge's labels.
  expect_error(
    calibrate_judge(gold, label_levels = c("A", "C"), n_boot = 20, seed = 1),
    "`judge_label` returned label outside `label_levels`"
  )
})

test_that("a factor judge column's levels join the inferred universe", {
  gold <- tibble::tibble(
    human_label = rep(c("A", "B"), 10),
    judge_label = factor(rep(c("A", "B"), 10), levels = c("A", "B", "C"))
  )
  cal <- calibrate_judge(gold, n_boot = 20, seed = 1)
  expect_identical(cal$label_levels, c("A", "B", "C"))
})

test_that("a live judge's labels are inferred after grading", {
  gold <- tibble::tibble(text = letters[1:20], human_label = rep("A", 20))
  judge <- function(d) c("B", rep("A", nrow(d) - 1))
  cal <- calibrate_judge(
    gold, judge = judge, input_cols = "text", n_boot = 20, seed = 1
  )
  expect_setequal(cal$label_levels, c("A", "B"))
  expect_error(
    calibrate_judge(
      gold, judge = judge, input_cols = "text", label_levels = c("A", "C"),
      n_boot = 20, seed = 1
    ),
    "`judge` returned label outside `label_levels`"
  )

  factor_judge <- function(d) {
    factor(rep(c("A", "B"), length.out = nrow(d)), levels = c("A", "B", "C"))
  }
  cal <- calibrate_judge(
    gold, judge = factor_judge, input_cols = "text", n_boot = 20, seed = 1
  )
  expect_identical(cal$label_levels, c("A", "B", "C"))
})

test_that("structured judges may return keys as their exact string forms", {
  gold <- tibble::tibble(
    text = letters[1:20], human_label = rep(c("A", "B"), 10)
  )
  structured <- function(key) {
    function(d) data.frame(.item_id = key(d$.item_id), label = "A")
  }
  as_int <- calibrate_judge(
    gold, judge = structured(identity), input_cols = "text",
    n_boot = 20, seed = 1
  )
  as_chr <- calibrate_judge(
    gold, judge = structured(as.character), input_cols = "text",
    n_boot = 20, seed = 1
  )
  expect_identical(as_chr$judge_output, as_int$judge_output)
  expect_identical(as_chr$agreement, as_int$agreement)

  # Anything that is not exactly an issued key is refused, never re-keyed.
  padded <- structured(function(id) sprintf("%02d", id))
  expect_error(
    calibrate_judge(gold, judge = padded, input_cols = "text", n_boot = 20),
    "every input key exactly once"
  )
  fractional <- structured(function(id) id + 0.5)
  expect_error(
    calibrate_judge(gold, judge = fractional, input_cols = "text", n_boot = 20),
    "fractional or out-of-range key"
  )
  logical_keys <- structured(function(id) rep(TRUE, length(id)))
  expect_error(
    calibrate_judge(gold, judge = logical_keys, input_cols = "text", n_boot = 20),
    "keys must be the issued integer keys"
  )
})

test_that("a repeated coefficient is rejected", {
  gold <- tibble::tibble(
    human_label = rep(c("A", "B"), 10), judge_label = rep(c("A", "B"), 10)
  )
  expect_error(
    calibrate_judge(gold, coefficients = c("gwet_ac1", "gwet_ac1"), n_boot = 20),
    "must not repeat a coefficient"
  )
  expect_error(
    agreement(gold, coefficients = c("cohen_kappa", "gwet_ac1", "cohen_kappa")),
    "must not repeat a coefficient"
  )
})

test_that("agreement bands follow Landis and Koch at every edge", {
  edges <- c(
    -1, -0.01, 0, 0.2, 0.21, 0.4, 0.41, 0.6, 0.61, 0.8, 0.81, 1
  )
  expect_identical(
    vapply(edges, interpret_agreement, character(1)),
    c(
      "poor", "poor", "slight", "slight", "fair", "fair", "moderate",
      "moderate", "substantial", "substantial", "almost perfect",
      "almost perfect"
    )
  )
  expect_identical(interpret_agreement(NA_real_), NA_character_)
})

test_that("all-missing ratings name the actual problem", {
  ratings <- data.frame(a = c(NA_character_, NA), b = c(NA_character_, NA))
  err <- rlang::catch_cnd(agreement(ratings), "error")
  expect_match(conditionMessage(err), "no non-missing ratings")
  expect_no_match(conditionMessage(err), "label_levels")
})
