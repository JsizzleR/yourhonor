test_that("a deterministic judge is perfectly stable", {
  det_judge <- function(d) rep("C", nrow(d))
  data <- tibble::tibble(input = paste("answer", 1:10))
  st <- test_retest(data, det_judge, n_runs = 4, seed = 1)
  expect_s3_class(st, "yourhonor_stability")
  expect_equal(st$flip_rate, 0)
  expect_equal(st$percent_agreement, 1)
  expect_equal(st$stability, 1)
  expect_equal(nrow(st$per_item), 10)
})

test_that("chance-corrected stability is below raw agreement for a noisy judge", {
  noisy_judge <- function(d) sample(c("C", "P"), nrow(d), replace = TRUE)
  data <- tibble::tibble(input = paste("answer", 1:60))
  st <- test_retest(data, noisy_judge, n_runs = 5, seed = 1)
  expect_gt(st$flip_rate, 0)
  # a pure-noise 2-label judge: percent agreement ~0.5 but AC1 ~0
  expect_lt(st$stability, st$percent_agreement)
  expect_lt(st$stability, 0.3)
})

test_that("ties produce NA modal and a tie flag", {
  expect_true(is.na(yourhonor:::modal_value(c("C", "P"))))
  expect_false(is.na(yourhonor:::modal_value(c("C", "C", "P"))))
  expect_true(yourhonor:::is_tie(c("C", "P")))
  expect_false(yourhonor:::is_tie(c("C", "C", "P")))
})

test_that("stability print shows all three numbers", {
  det_judge <- function(d) rep("C", nrow(d))
  data <- tibble::tibble(input = paste("answer", 1:5))
  st <- test_retest(data, det_judge, n_runs = 3)
  out <- cli::cli_fmt(print(st))
  txt <- paste(out, collapse = " ")
  expect_match(txt, "test-retest stability")
  expect_match(txt, "percent")
})
