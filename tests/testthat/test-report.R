make_cal <- function() {
  gold <- tibble::tibble(
    human_label = rep(c("C", "P", "I"), times = c(12, 4, 4)),
    judge_label = c(rep("C", 12), "C", "P", "P", "P", "I", "I", "P", "I")
  )
  calibrate_judge(gold, n_boot = 100, seed = 1)
}

test_that("report markdown contains a headline and a verdict", {
  md <- report(make_cal(), output = "md")
  expect_s3_class(md, "yourhonor_report")
  txt <- paste(unclass(md), collapse = "\n")
  expect_match(txt, "Agreement with humans")
  expect_match(txt, "Overall:")
})

test_that("console report prints a verdict", {
  out <- cli::cli_fmt(report(make_cal(), output = "console"))
  expect_match(paste(out, collapse = "\n"), "yourhonor verdict")
})

test_that("overall_verdict thresholds behave", {
  expect_equal(yourhonor:::overall_verdict(0.85, TRUE, 0.05), "USE")
  expect_equal(yourhonor:::overall_verdict(0.85, FALSE, 0.05), "USE WITH CAUTION")
  expect_equal(yourhonor:::overall_verdict(0.70, TRUE, 0.05), "USE WITH CAUTION")
  expect_equal(yourhonor:::overall_verdict(0.40, TRUE, 0.05), "NOT CALIBRATED")
  # instability downgrades an otherwise strong judge
  expect_equal(yourhonor:::overall_verdict(0.85, TRUE, 0.30), "USE WITH CAUTION")
})

test_that("report flags a significant bias", {
  bias <- tibble::tibble(
    probe = "verbosity", flip_rate = 0.3, direction = "favors_perturbed",
    statistic = 9L, n_discordant = 10L, p_value = 0.001, verdict = "BIAS"
  )
  bias <- structure(bias, class = c("yourhonor_bias", class(bias)))
  txt <- paste(unclass(report(make_cal(), bias = bias, output = "md")), collapse = "\n")
  expect_match(txt, "Significant bias")
})
