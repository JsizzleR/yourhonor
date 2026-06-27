test_that("perfect agreement is near 1 for every coefficient", {
  ratings <- data.frame(
    human = rep(c("C", "P", "I"), each = 6),
    judge = rep(c("C", "P", "I"), each = 6)
  )
  ag <- agreement(ratings, n_boot = 50, seed = 1)
  expect_setequal(ag$coefficient, yourhonor:::yourhonor_coefficients)
  expect_true(all(ag$estimate > 0.99))
})

test_that("agreement returns one tidy row per requested coefficient", {
  ratings <- data.frame(
    human = c("C", "C", "P", "I", "C", "P", "I", "C"),
    judge = c("C", "C", "C", "I", "C", "P", "P", "C")
  )
  ag <- agreement(ratings, coefficients = c("gwet_ac1", "cohen_kappa"),
                  n_boot = 100, seed = 1)
  expect_s3_class(ag, "tbl_df")
  expect_setequal(ag$coefficient, c("gwet_ac1", "cohen_kappa"))
  expect_named(
    ag,
    c("coefficient", "estimate", "conf_low", "conf_high", "se",
      "n_items", "n_raters", "interpretation")
  )
  expect_true(all(ag$conf_low <= ag$estimate + 1e-8))
  expect_true(all(ag$conf_high >= ag$estimate - 1e-8))
})

test_that("bootstrap is reproducible given a seed and does not leak RNG state", {
  ratings <- data.frame(
    human = c("C", "C", "P", "I", "C", "P", "I", "C"),
    judge = c("C", "C", "C", "I", "C", "P", "P", "C")
  )
  a <- agreement(ratings, n_boot = 100, seed = 99)
  b <- agreement(ratings, n_boot = 100, seed = 99)
  expect_equal(a, b)

  # the caller's global stream must be untouched
  set.seed(123)
  before <- runif(1)
  set.seed(123)
  invisible(agreement(ratings, n_boot = 50, seed = 7))
  after <- runif(1)
  expect_equal(before, after)
})

test_that("invalid coefficient names are rejected", {
  ratings <- data.frame(human = c("C", "P"), judge = c("C", "P"))
  expect_error(agreement(ratings, coefficients = "not_a_coef"))
})

test_that("interpret_agreement bands fall in the conventional Landis-Koch band", {
  expect_equal(yourhonor:::interpret_agreement(0.81), "almost perfect")
  expect_equal(yourhonor:::interpret_agreement(0.80), "substantial")
  expect_equal(yourhonor:::interpret_agreement(0.60), "moderate")
  expect_equal(yourhonor:::interpret_agreement(0.40), "fair")
  expect_true(is.na(yourhonor:::interpret_agreement(NA_real_)))
})

test_that("NA rows are dropped with a warning and too-few rows error", {
  ratings <- data.frame(
    human = c("C", "C", NA, "I"),
    judge = c("C", "P", "C", "I")
  )
  expect_warning(ag <- agreement(ratings, n_boot = 20, seed = 1), "Dropped")
  expect_equal(ag$n_items[1], 3)
  expect_error(
    suppressWarnings(agreement(data.frame(human = c("C", NA), judge = c("C", "P")))),
    "at least 2"
  )
})

test_that("empty ratings error cleanly", {
  expect_error(
    agreement(data.frame(human = character(0), judge = character(0))),
    "at least 2"
  )
})
