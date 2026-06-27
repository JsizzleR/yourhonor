test_that("a length-sensitive judge shows verbosity bias", {
  length_judge <- function(d) ifelse(nchar(d$input) > 40, "C", "P")
  data <- tibble::tibble(input = paste("answer", 1:30))
  res <- probe_bias(data, length_judge, probes = "verbosity", seed = 1)

  expect_s3_class(res, "yourhonor_bias")
  expect_equal(res$probe, "verbosity")
  expect_gt(res$flip_rate, 0)
  expect_equal(res$direction, "favors_perturbed")
  expect_equal(res$verdict, "BIAS")
})

test_that("self_preference is NOT confounded with verbosity (regression)", {
  # purely length-sensitive judge: must trip verbosity but NOT self_preference,
  # because the self-preference prefixes are equal length.
  length_judge <- function(d) ifelse(nchar(d$input) > 40, "C", "P")
  data <- tibble::tibble(input = paste("answer", 1:30))
  res <- probe_bias(data, length_judge,
                    probes = c("verbosity", "self_preference"), seed = 1)
  v <- res[res$probe == "verbosity", ]
  s <- res[res$probe == "self_preference", ]
  expect_equal(v$verdict, "BIAS")
  expect_equal(s$flip_rate, 0)
  expect_equal(s$verdict, "ok")
})

test_that("an invariant judge shows no bias", {
  flat_judge <- function(d) rep("C", nrow(d))
  data <- tibble::tibble(input = paste("answer", 1:20))
  res <- probe_bias(data, flat_judge, probes = c("verbosity", "self_preference"),
                    seed = 1)
  expect_true(all(res$flip_rate == 0))
  expect_true(all(res$verdict == "ok"))
})

test_that("probe_bias requires a judge function and an input column", {
  data <- tibble::tibble(input = "x")
  expect_error(probe_bias(data, NULL), "must be a function")
  expect_error(
    probe_bias(tibble::tibble(text = "x"), function(d) "C"),
    "input"
  )
})

test_that("a judge on a different label scale aborts rather than passing 'ok'", {
  data <- tibble::tibble(input = paste("answer", 1:10))
  good_bad <- function(d) ifelse(nchar(d$input) > 40, "good", "bad")
  expect_error(
    probe_bias(data, good_bad, probes = "verbosity", seed = 1),
    "ordered_levels"
  )
})

test_that("paired_flip_test handles no discordant pairs", {
  res <- yourhonor:::paired_flip_test(c(1, 2, 3), c(1, 2, 3))
  expect_equal(res$flip_rate, 0)
  expect_equal(res$direction, "none")
  expect_true(is.na(res$p_value))
})

test_that("bandwagon probe catches a crowd-following judge", {
  # judge follows the claimed crowd verdict embedded in the prompt
  crowd_judge <- function(d) ifelse(grepl("looks right", d$input), "C", "P")
  data <- tibble::tibble(input = paste("answer", 1:30))
  res <- probe_bias(data, crowd_judge, probes = "bandwagon", seed = 1)
  expect_equal(res$probe, "bandwagon")
  expect_gt(res$flip_rate, 0)
  expect_equal(res$direction, "favors_perturbed")
  expect_equal(res$verdict, "BIAS")
})

test_that("bandwagon is length-matched: a length-only judge is not flagged", {
  length_judge <- function(d) ifelse(nchar(d$input) > 40, "C", "P")
  data <- tibble::tibble(input = paste("answer", 1:30))
  res <- probe_bias(data, length_judge, probes = "bandwagon", seed = 1)
  expect_equal(res$flip_rate, 0)
  expect_equal(res$verdict, "ok")
})

test_that("p-values are corrected across probes", {
  flat_judge <- function(d) rep("C", nrow(d))
  data <- tibble::tibble(input = paste("answer", 1:20))
  res <- probe_bias(data, flat_judge,
                    probes = c("verbosity", "self_preference", "bandwagon"),
                    seed = 1)
  expect_true("p_value_adj" %in% names(res))
  expect_equal(nrow(res), 3)
  # holm-adjusted p is never smaller than the raw p
  ok <- !is.na(res$p_value)
  expect_true(all(res$p_value_adj[ok] >= res$p_value[ok] - 1e-9))
})
