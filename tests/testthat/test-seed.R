# Every public seed argument goes through one validator, so a seed that
# set.seed() would truncate, coerce or partly ignore is refused rather than
# recorded as if it had been used.

seed_ratings <- function() {
  tibble::tibble(
    human = rep(c("I", "P", "C"), times = 4),
    judge = rep(c("I", "P", "C"), times = 4)
  )
}

seed_vitals_epochs <- function() {
  tibble::tibble(
    id = rep(c("a", "b", "c"), each = 2),
    epoch = rep(1:2, times = 3),
    input = rep(paste("input", 1:3), each = 2),
    score = rep(c("I", "P", "C"), each = 2)
  )
}

bad_seeds <- list(
  vector = c(7, 99), fraction = 1.5, logical = TRUE, string = "abc",
  missing = NA_real_, infinite = Inf, too_large = 2^31, complex = 1i,
  factor = factor("1")
)

seed_calls <- list(
  agreement = function(seed) {
    agreement(seed_ratings(), n_boot = 30, seed = seed)
  },
  calibrate_judge = function(seed) {
    calibrate_judge(
      seed_ratings(), human_cols = "human", judge_col = "judge",
      n_boot = 30, seed = seed
    )
  },
  probe_bias = function(seed) {
    probe_bias(
      tibble::tibble(input = paste("answer", 1:6)),
      function(d) rep("P", nrow(d)), probes = "verbosity", seed = seed
    )
  },
  probe_position = function(seed) {
    probe_position(
      tibble::tibble(a = paste("A", 1:6), b = paste("B", 1:6)),
      function(d) tibble::tibble(.item_id = d$.item_id, label = "A"),
      "a", "b", seed = seed
    )
  },
  audit_vitals = function(seed) {
    audit_vitals(
      tibble::tibble(id = c("a", "b", "c"), score = c("I", "P", "C")),
      human_labels = c("I", "P", "C"), n_boot = 30, seed = seed
    )
  },
  stability_vitals = function(seed) {
    stability_vitals(
      seed_vitals_epochs(), content_cols = "input", n_boot = 30, seed = seed
    )
  }
)

test_that("every seed argument refuses a value set.seed() would not use as given", {
  for (fn in names(seed_calls)) {
    for (kind in names(bad_seeds)) {
      # expect_error() alone would pass with a coercion warning escaping
      # beside the error, which is the "abc" defect itself.
      warned <- FALSE
      withCallingHandlers(
        expect_error(
          seed_calls[[fn]](bad_seeds[[kind]]),
          "`seed` must be `NULL` or one whole number",
          label = paste(fn, kind)
        ),
        warning = function(w) {
          warned <<- TRUE
          invokeRestart("muffleWarning")
        }
      )
      expect_false(warned, label = paste(fn, kind, "warned"))
    }
  }
})

test_that("a whole-number seed is accepted and recorded as an integer", {
  cal <- seed_calls$calibrate_judge(7)
  expect_identical(cal$seed, 7L)
  expect_identical(seed_calls$calibrate_judge(7L)$seed, 7L)
  expect_identical(seed_calls$calibrate_judge(-3)$seed, -3L)
  expect_null(seed_calls$calibrate_judge(NULL)$seed)
  stab <- seed_calls$stability_vitals(7)
  expect_identical(stab$provenance$seed, 7L)
  # The seed is still honoured: the same seed gives the same draws.
  expect_identical(
    seed_calls$agreement(7)$conf_low, seed_calls$agreement(7L)$conf_low
  )
})
