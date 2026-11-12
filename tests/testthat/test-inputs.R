# Regression tests for input-handling findings from the whole-codebase review.

test_that("inferred label levels include labels only the judge used", {
  gold <- tibble::tibble(
    human_label = rep("A", 20),
    judge_label = c("B", rep("A", 19))
  )
  cal <- calibrate_judge(gold, n_boot = 20, seed = 1)
  expect_identical(cal$label_levels, c("A", "B"))
  expect_all(cal$agreement$status == "insufficient_reference_variation")

  # Supplied levels are still a contract on the judge's labels.
  expect_error(
    calibrate_judge(gold, label_levels = c("A", "C"), n_boot = 20, seed = 1),
    "`judge_label` returned label outside `label_levels`",
    class = "yourhonor_input_error"
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
    "`judge` returned label outside `label_levels`",
    class = "yourhonor_judge_error"
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
    class = "yourhonor_judge_error"
  )
  fractional <- structured(function(id) id + 0.5)
  expect_error(
    calibrate_judge(gold, judge = fractional, input_cols = "text", n_boot = 20),
    class = "yourhonor_judge_error"
  )
  logical_keys <- structured(function(id) rep(TRUE, length(id)))
  expect_error(
    calibrate_judge(gold, judge = logical_keys, input_cols = "text", n_boot = 20),
    class = "yourhonor_judge_error"
  )
})

test_that("a repeated coefficient is rejected", {
  gold <- tibble::tibble(
    human_label = rep(c("A", "B"), 10), judge_label = rep(c("A", "B"), 10)
  )
  expect_error(
    calibrate_judge(gold, coefficients = c("gwet_ac1", "gwet_ac1"), n_boot = 20),
    class = "yourhonor_input_error"
  )
  expect_error(
    agreement(gold, coefficients = c("cohen_kappa", "gwet_ac1", "cohen_kappa")),
    class = "yourhonor_input_error"
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
  err <- rlang::catch_cnd(
    agreement(ratings, rater_cols = c("a", "b")), "error"
  )
  expect_match(conditionMessage(err), "no non-missing ratings")
  expect_no_match(conditionMessage(err), "label_levels")
})

test_that("ordinal label_levels must follow an ordered factor's order", {
  # support_gold's label columns are ordered I < P < C.
  expect_warning(
    scrambled <- calibrate_judge(
      support_gold, scale = "ordinal", label_levels = c("C", "I", "P"),
      n_boot = 20, seed = 1
    ),
    "human_label.*I < P < C"
  )
  expect_identical(scrambled$label_levels, c("C", "I", "P"))
  # Forwards and reversed are both order-consistent; so is a superset.
  for (levels in list(c("I", "P", "C"), c("C", "P", "I"), c("I", "X", "P", "C"))) {
    expect_no_warning(
      calibrate_judge(
        support_gold, scale = "ordinal", label_levels = levels,
        n_boot = 20, seed = 1
      )
    )
  }
  # A nominal scale has no order to contradict.
  expect_no_warning(
    calibrate_judge(
      support_gold, label_levels = c("C", "I", "P"), n_boot = 20, seed = 1
    )
  )
  # The precomputed judge column is checked as well as the human columns.
  judge_only <- support_gold
  judge_only$human_label <- as.character(judge_only$human_label)
  expect_warning(
    calibrate_judge(
      judge_only, scale = "ordinal", label_levels = c("P", "I", "C"),
      n_boot = 20, seed = 1
    ),
    "judge_label"
  )
  # Without label_levels the error names the order an ordered factor declares.
  expect_error(
    calibrate_judge(support_gold, scale = "ordinal", n_boot = 20),
    "I < P < C", class = "yourhonor_input_error"
  )
})

test_that("label and key-column messages pluralise on their counts", {
  # `{?s}` takes its quantity from the nearest substitution; these two once
  # pluralised on the argument name, so two labels read "label".
  expect_error(
    validate_labels(c("x", "y", "a"), c("a", "b")),
    "`judge` returned labels outside `label_levels`: \"x\" and \"y\"",
    class = "yourhonor_input_error"
  )
  expect_error(
    validate_labels(c("x", "a"), c("a", "b")),
    "`judge` returned label outside `label_levels`: \"x\"\\.",
    class = "yourhonor_input_error"
  )

  data <- tibble::tibble(a = 1:3, b = 4:6, text = letters[1:3])
  no_keys <- function(d) tibble::tibble(label = rep("A", nrow(d)))
  expect_error(
    grade_keyed(no_keys, data, key_cols = c("a", "b")),
    "must contain label and key columns: a and b\\.",
    class = "yourhonor_judge_error"
  )
  expect_error(
    grade_keyed(no_keys, data, key_cols = "a"),
    "must contain label and key column: a\\.",
    class = "yourhonor_judge_error"
  )
})
