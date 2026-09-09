test_that("perfect varied agreement is one for every coefficient", {
  ratings <- data.frame(
    human = rep(c("I", "P", "C"), each = 6),
    judge = rep(c("I", "P", "C"), each = 6)
  )
  ag <- agreement(ratings, n_boot = 50, seed = 1,
                  label_levels = c("I", "P", "C"))
  expect_setequal(ag$coefficient, yourhonor:::yourhonor_coefficients)
  expect_true(all(ag$estimate > 0.99))
  expect_true(all(ag$status == "ok"))
  expect_true(all(ag$n_boot_effective > 0))
  expect_true(all(ag$boundary_degenerate))
  expect_true(all(!ag$ci_reliable))
})

test_that("agreement returns explicit validity metadata", {
  ratings <- data.frame(
    human = c("C", "C", "P", "I", "C", "P", "I", "C"),
    judge = c("C", "C", "C", "I", "C", "P", "P", "C")
  )
  ag <- agreement(
    ratings, coefficients = c("gwet_ac1", "cohen_kappa"),
    n_boot = 100, seed = 1, label_levels = c("I", "P", "C")
  )
  expect_s3_class(ag, "tbl_df")
  expect_setequal(ag$coefficient, c("gwet_ac1", "cohen_kappa"))
  expect_true(all(c(
    "status", "ci_reliable", "boundary_degenerate", "n_boot_effective",
    "n_missing_rows", "weights", "interpretation"
  ) %in% names(ag)))
  expect_true(all(ag$conf_low <= ag$estimate + 1e-8))
  expect_true(all(ag$conf_high >= ag$estimate - 1e-8))
})

test_that("bootstrap is reproducible and does not leak RNG state", {
  ratings <- data.frame(
    human = c("C", "C", "P", "I", "C", "P", "I", "C"),
    judge = c("C", "C", "C", "I", "C", "P", "P", "C")
  )
  a <- agreement(ratings, n_boot = 100, seed = 99)
  b <- agreement(ratings, n_boot = 100, seed = 99)
  expect_equal(a, b)

  set.seed(123)
  before <- runif(1)
  set.seed(123)
  invisible(agreement(ratings, n_boot = 50, seed = 7))
  after <- runif(1)
  expect_equal(before, after)
})

test_that("one bootstrap index set is shared across coefficients", {
  ratings <- data.frame(
    human = c("C", "C", "P", "I", "C", "P", "I", "C"),
    judge = c("C", "C", "C", "I", "C", "P", "P", "C")
  )
  together <- agreement(
    ratings, coefficients = c("cohen_kappa", "gwet_ac1"),
    n_boot = 60, seed = 5, label_levels = c("I", "P", "C")
  )
  alone <- agreement(
    ratings, coefficients = "gwet_ac1",
    n_boot = 60, seed = 5, label_levels = c("I", "P", "C")
  )
  # AC1 sees the same draws whether or not kappa ran before it.
  expect_equal(
    together[together$coefficient == "gwet_ac1", c("conf_low", "conf_high")],
    alone[, c("conf_low", "conf_high")]
  )
})

test_that("invalid coefficient and interval controls are rejected", {
  ratings <- data.frame(human = c("C", "P"), judge = c("C", "P"))
  expect_error(agreement(ratings, coefficients = "not_a_coef"))
  expect_error(agreement(ratings, n_boot = 0), "n_boot")
  expect_error(agreement(ratings, conf_level = 1), "conf_level")
  expect_error(
    agreement(ratings, label_levels = c("C", "C")),
    "unique"
  )
})

test_that("check helpers report the user-facing call", {
  ratings <- data.frame(human = c("C", "P"), judge = c("C", "P"))
  err <- rlang::catch_cnd(agreement(ratings, n_boot = 0))
  expect_identical(rlang::call_name(err$call), "agreement")
  err <- rlang::catch_cnd(agreement(ratings, conf_level = 2))
  expect_identical(rlang::call_name(err$call), "agreement")
})

test_that("interpretation bands remain descriptive", {
  expect_equal(yourhonor:::interpret_agreement(0.81), "almost perfect")
  expect_equal(yourhonor:::interpret_agreement(0.80), "substantial")
  expect_equal(yourhonor:::interpret_agreement(0.60), "moderate")
  expect_equal(yourhonor:::interpret_agreement(0.40), "fair")
  expect_true(is.na(yourhonor:::interpret_agreement(NA_real_)))
})

test_that("partially observed multi-rater rows are retained", {
  ratings <- data.frame(
    r1 = c("C", "C", NA, "P"),
    r2 = c("C", NA, "P", "P"),
    r3 = c("C", "C", "P", NA)
  )
  ag <- agreement(
    ratings,
    coefficients = c("krippendorff_alpha", "gwet_ac1"),
    n_boot = 50,
    seed = 1,
    label_levels = c("P", "C"),
    rater_cols = c("r1", "r2", "r3")
  )
  expect_equal(ag$n_items, c(4L, 4L))
  expect_equal(ag$n_missing_rows, c(3L, 3L))
  expect_true(all(ag$status == "ok"))
})

test_that("single-class data is explicitly indeterminate", {
  ratings <- data.frame(human = rep("C", 20), judge = rep("C", 20))
  ag <- agreement(ratings, n_boot = 50, label_levels = c("I", "C"))
  expect_true(all(is.na(ag$estimate)))
  expect_true(all(ag$status == "insufficient_variation"))
  expect_true(all(!ag$ci_reliable))
})

test_that("cohen_kappa reports invalid_rater_count beyond two raters", {
  ratings <- data.frame(
    r1 = c("C", "P", "C", "P"),
    r2 = c("C", "P", "P", "P"),
    r3 = c("C", "C", "C", "P")
  )
  ag <- agreement(
    ratings, coefficients = c("cohen_kappa", "gwet_ac1"), n_boot = 20,
    seed = 1, rater_cols = c("r1", "r2", "r3")
  )
  expect_identical(
    ag$status[ag$coefficient == "cohen_kappa"], "invalid_rater_count"
  )
  expect_true(is.na(ag$estimate[ag$coefficient == "cohen_kappa"]))
  expect_identical(ag$status[ag$coefficient == "gwet_ac1"], "ok")
  expect_equal(ag$n_raters, c(3L, 3L))
})

test_that("fewer than two usable rows is insufficient_items", {
  ag <- agreement(
    data.frame(human = c("C", NA), judge = c("C", "P")), n_boot = 20
  )
  expect_true(all(ag$status == "insufficient_items"))
  expect_equal(ag$n_items, rep(1L, nrow(ag)))
  expect_true(all(is.na(ag$estimate)))
  expect_true(all(!ag$ci_reliable))
})

test_that("intervals from too few finite resamples are unreliable", {
  ratings <- data.frame(human = c(rep("C", 9), "P"), judge = rep("C", 10))
  ag <- agreement(
    ratings, label_levels = c("P", "C"), n_boot = 200, seed = 1
  )
  expect_true(all(ag$status == "ok"))
  expect_true(all(is.finite(ag$estimate)))
  # Roughly a third of the draws omit the single "P" row and are undefined.
  expect_true(all(ag$n_boot_effective < ceiling(0.8 * 200)))
  expect_true(all(ag$n_boot_effective >= 30L))
  expect_true(all(!ag$ci_reliable))
  expect_true(all(!ag$boundary_degenerate))
})

test_that("ordinal weights distinguish adjacent and extreme mistakes", {
  adjacent <- data.frame(human = c("I", "P", "C"), judge = c("P", "C", "C"))
  extreme <- data.frame(human = c("I", "P", "C"), judge = c("C", "I", "C"))
  a <- agreement(
    adjacent, coefficients = "cohen_kappa", weights = "quadratic",
    label_levels = c("I", "P", "C"), n_boot = 50, seed = 1
  )
  b <- agreement(
    extreme, coefficients = "cohen_kappa", weights = "quadratic",
    label_levels = c("I", "P", "C"), n_boot = 50, seed = 1
  )
  expect_gt(a$estimate, b$estimate)
})

test_that("weighted Gwet is named AC2 and requires declared order", {
  ratings <- data.frame(
    human = c("I", "P", "C", "I", "P", "C"),
    judge = c("P", "P", "C", "I", "C", "C")
  )
  expect_error(agreement(ratings, weights = "linear"), "label_levels")
  expect_error(
    agreement(
      ratings, coefficients = "gwet_ac1", weights = "linear",
      label_levels = c("I", "P", "C")
    ),
    "AC2"
  )
  ag <- agreement(
    ratings, weights = "linear", label_levels = c("I", "P", "C"),
    n_boot = 50, seed = 1
  )
  expect_true("gwet_ac2" %in% ag$coefficient)
  expect_false("gwet_ac1" %in% ag$coefficient)
})

test_that("coefficients = NULL is the auto rule, whether omitted or passed", {
  ratings <- data.frame(
    human = c("I", "P", "C", "I", "P", "C"),
    judge = c("P", "P", "C", "I", "C", "C")
  )
  args <- list(
    x = ratings, weights = "linear", label_levels = c("I", "P", "C"),
    n_boot = 30, seed = 1
  )
  omitted <- do.call(agreement, args)
  explicit <- do.call(agreement, c(args, list(coefficients = NULL)))
  expect_equal(omitted, explicit)
  expect_setequal(
    omitted$coefficient, c("cohen_kappa", "krippendorff_alpha", "gwet_ac2")
  )
  expect_error(
    agreement(ratings, coefficients = "gwet_ac2", n_boot = 20),
    "non-default"
  )
})

test_that("agreement on a calibration inherits every default", {
  gold <- tibble::tibble(
    input = paste("answer", 1:8),
    human_label = c("C", "C", "P", "I", "C", "P", "I", "C"),
    judge_label = c("C", "C", "C", "I", "C", "P", "P", "C")
  )
  cal <- calibrate_judge(
    gold, label_levels = c("I", "P", "C"), conf_level = 0.9,
    n_boot = 40, seed = 11
  )
  again <- agreement(cal, coefficients = "gwet_ac1")
  direct <- agreement(
    cal$ratings, coefficients = "gwet_ac1", conf_level = 0.9,
    n_boot = 40, seed = 11, label_levels = c("I", "P", "C")
  )
  expect_equal(again, direct)
  expect_equal(again, cal$agreement[cal$agreement$coefficient == "gwet_ac1", ])

  first <- agreement(cal, n_boot = 30)
  second <- agreement(cal, n_boot = 30)
  expect_equal(first, second)
  expect_error(agreement(cal, rater_cols = c("human", "judge")), "rater_cols")
})

test_that("rater columns are explicit or inferred with a message", {
  wide <- data.frame(
    id = 1:4, human = c("C", "P", "C", "P"), judge = c("C", "P", "P", "P"),
    note = "x"
  )
  expect_message(
    ag <- agreement(wide, n_boot = 20, seed = 1),
    class = "yourhonor_inferred_columns"
  )
  expect_equal(unique(ag$n_raters), 2L)

  expect_silent(
    explicit <- agreement(
      wide, rater_cols = c("human", "judge"), n_boot = 20, seed = 1
    )
  )
  expect_equal(explicit, ag)

  unnamed <- data.frame(a = c("C", "P", "C"), b = c("C", "P", "P"))
  expect_message(
    all_cols <- agreement(unnamed, n_boot = 20),
    "Treating all 2 columns"
  )
  expect_equal(unique(all_cols$n_raters), 2L)

  expect_error(agreement(wide, rater_cols = "human"), "rater_cols")
  expect_error(agreement(wide, rater_cols = c("human", "nope")), "nope")
})

test_that("too few rated rows error cleanly", {
  expect_error(
    agreement(data.frame(human = character(0), judge = character(0))),
    "at least 2"
  )
})

test_that("labels that differ only by case or whitespace stay distinct", {
  # irrCAC upper-cases and trims character ratings; ratings are integer-coded
  # before it sees them so the package's exact-match universe is preserved.
  case_swaps <- data.frame(
    human = c("C", "c", "C", "c", "C", "c"),
    judge = c("C", "c", "c", "C", "C", "c")
  )
  ag <- agreement(case_swaps, n_boot = 20, seed = 1)
  expect_true(all(ag$status == "ok"))
  expect_equal(ag$estimate[ag$coefficient == "cohen_kappa"], 1 / 3, tolerance = 1e-4)
  expect_equal(ag$estimate[ag$coefficient == "gwet_ac1"], 1 / 3, tolerance = 1e-4)

  padded <- data.frame(
    human = c("C", "P", "C", "P", "C", "P"),
    judge = c("C ", "P", "C", "P ", "C", "P")
  )
  ag <- agreement(padded, n_boot = 20, seed = 1)
  conf <- confusion(padded)
  expect_true(all(ag$estimate < 1))
  expect_equal(sum(conf$n[conf$human_label != conf$judge_label]), 2)
})

test_that("an unused blank factor level is ignored, not fatal", {
  ratings <- data.frame(
    human = factor(rep(c("C", "I", "P"), 4), levels = c("", "C", "I", "P")),
    judge = rep(c("C", "I", "P"), length.out = 12)
  )
  ag <- agreement(ratings, n_boot = 20, seed = 1)
  expect_true(all(ag$status == "ok"))
  expect_setequal(
    yourhonor:::resolve_label_levels(ratings, NULL), c("C", "I", "P")
  )
})

test_that("empty-string labels are rejected as missing values", {
  blank_judge <- data.frame(
    human = c("C", "P", "C", "P", "C", "P"),
    judge = c("C", "", "C", "P", "", "P")
  )
  expect_error(agreement(blank_judge, n_boot = 10), "empty or whitespace-only")
  expect_error(confusion(blank_judge), "empty or whitespace-only")
  expect_error(
    agreement(blank_judge[c(1, 3, 4, 6), ], label_levels = c("C", "P", " ")),
    "empty or whitespace-only"
  )
})

test_that("factor levels are unioned with labels observed in character columns", {
  ratings <- data.frame(
    human = factor(c("a", "b", "a", "b")),
    judge = c("a", "c", "a", "b"),
    stringsAsFactors = FALSE
  )
  expect_identical(yourhonor:::resolve_label_levels(ratings, NULL), c("a", "b", "c"))
  conf <- confusion(ratings)
  expect_equal(sum(conf$n), 4)
  ag <- agreement(ratings, n_boot = 20, seed = 1)
  expect_true(all(ag$status == "ok"))
  expect_equal(ag$n_items, rep(4L, nrow(ag)))
})
