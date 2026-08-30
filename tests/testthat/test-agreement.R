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
    "n_missing_rows", "weights"
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
    label_levels = c("P", "C")
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

test_that("too few rated rows error cleanly", {
  expect_error(
    agreement(data.frame(human = character(0), judge = character(0))),
    "at least 2"
  )
})
