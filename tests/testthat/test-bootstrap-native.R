# The bootstrap scores resamples natively; irrCAC, through
# safe_coeff_point(), is the oracle every native value must equal.

# The bootstrap as it was: one irrCAC call per resample per coefficient.
irrcac_bootstrap <- function(ratings, coefficients, n_boot, weights,
                             label_levels, chance_levels = label_levels) {
  draws <- rlang::set_names(
    lapply(coefficients, function(co) rep(NA_real_, n_boot)), coefficients
  )
  n <- nrow(ratings)
  for (i in seq_len(n_boot)) {
    samp <- ratings[sample.int(n, n, replace = TRUE), , drop = FALSE]
    if (length(observed_categories(samp)) < 2L) next
    for (co in coefficients) {
      draws[[co]][[i]] <- safe_coeff_point(
        samp, co, weights, label_levels, chance_levels
      )
    }
  }
  draws
}

oracle_values <- function(ratings, idx, coefficients, weights, label_levels,
                          chance_levels) {
  lapply(rlang::set_names(coefficients), function(co) {
    vapply(seq_len(ncol(idx)), function(j) {
      samp <- ratings[idx[, j], , drop = FALSE]
      if (length(observed_categories(samp)) < 2L) return(NA_real_)
      safe_coeff_point(samp, co, weights, label_levels, chance_levels)
    }, numeric(1))
  })
}

# A random usable sample: rows with at least two ratings, possibly declared
# levels nobody used, and missing ratings when there are more than two raters.
random_usable <- function(n_items, n_raters, levels, used, missing = 0) {
  probs <- stats::runif(length(used)) + 0.2
  ratings <- as.data.frame(
    replicate(n_raters, sample(used, n_items, TRUE, probs), simplify = FALSE),
    col.names = paste0("r", seq_len(n_raters)), stringsAsFactors = FALSE
  )
  if (missing > 0 && n_raters > 2L) {
    holes <- matrix(stats::runif(n_items * n_raters) < missing, n_items)
    ratings[holes] <- NA
  }
  ratings[rowSums(!is.na(ratings)) >= 2L, , drop = FALSE]
}

scenario_coefficients <- function(weights, n_raters) {
  gwet <- if (weights == "unweighted") "gwet_ac1" else "gwet_ac2"
  c(if (n_raters == 2L) "cohen_kappa", "krippendorff_alpha", gwet)
}

test_that("native resample values equal irrCAC on random data", {
  # Count the resamples irrCAC actually scores inside the native path.
  oracle_calls <- 0L
  oracle <- safe_coeff_point
  local_mocked_bindings(safe_coeff_point = function(...) {
    oracle_calls <<- oracle_calls + 1L
    oracle(...)
  })
  set.seed(20261111)
  weight_schemes <- c("unweighted", "linear", "quadratic", "ordinal")
  scored <- 0L
  single_category <- 0L
  fallbacks <- 0L
  for (case in seq_len(48)) {
    weights <- weight_schemes[[(case - 1L) %% 4L + 1L]]
    n_levels <- sample(2:5, 1)
    levels <- as.character(seq_len(n_levels))
    # Some cases leave declared levels unused, and some have so few items
    # or so rare a category that resamples observe a single category.
    used <- if (n_levels > 2L && case %% 3L == 0L) {
      sort(sample(levels, n_levels - 1L))
    } else {
      levels
    }
    n_raters <- if (case %% 2L == 0L) 2L else sample(3:4, 1)
    n_items <- if (case %% 5L == 0L) sample(3:6, 1) else sample(8:60, 1)
    ratings <- random_usable(
      n_items, n_raters, levels, used,
      missing = if (case %% 4L < 2L) 0.3 else 0
    )
    chance <- chance_categories(ratings, levels)
    if (nrow(ratings) < 2L || length(chance) < 2L) next
    coefficients <- scenario_coefficients(weights, n_raters)
    idx <- replicate(40, sample.int(nrow(ratings), nrow(ratings), TRUE))
    kernel <- agreement_kernel(ratings, weights, levels, chance)
    before <- oracle_calls
    native <- native_resample_values(
      kernel, ratings, idx, coefficients, weights, levels, chance
    )
    fallbacks <- fallbacks + oracle_calls - before
    expected <- oracle_values(
      ratings, idx, coefficients, weights, levels, chance
    )
    expect_identical(native, expected, info = paste("case", case, weights))
    for (co in coefficients) {
      single_category <- single_category + sum(is.na(expected[[co]]))
      scored <- scored + sum(!is.na(expected[[co]]))
    }
  }
  # The comparison covered single-category resamples, and irrCAC scored only
  # the rare value the native arithmetic could not vouch for.
  expect_gt(single_category, 0)
  expect_gt(scored, 1000)
  expect_lt(fallbacks / scored, 0.01)
})

test_that("a value on a rounding boundary is scored by irrCAC", {
  # Krippendorff's alpha is 7/64 = 0.109375 here, halfway between two
  # five-decimal values: the native sums round it down to 0.10937 and
  # irrCAC's own sums round it up to 0.10938, so irrCAC must score it.
  ratings <- data.frame(
    r1 = c("a", "a", "a", "c", "b", "c", "a", "a", "a", "c"),
    r2 = c("b", "c", "a", "a", "b", "c", "c", "b", "c", "c")
  )
  levels <- c("a", "b", "c")
  expected <- safe_coeff_point(
    ratings, "krippendorff_alpha", "unweighted", levels, levels
  )
  expect_identical(expected, 0.10938)
  kernel <- agreement_kernel(ratings, "unweighted", levels, levels)
  raw <- agreement_kernel_draws(
    kernel, matrix(1, 10, 1), "krippendorff_alpha"
  )$krippendorff_alpha
  expect_true(raw$oracle)
  expect_false(identical(raw$value, expected))
  expect_identical(
    native_resample_values(
      kernel, ratings, matrix(seq_len(10), ncol = 1), "krippendorff_alpha",
      "unweighted", levels, levels
    )$krippendorff_alpha,
    expected
  )
  expect_true(near_rounding_boundary(0.109375, 1e-9))
  expect_false(near_rounding_boundary(0.10937, 1e-9))
  expect_false(near_rounding_boundary(NA_real_, 1e-9))
})

test_that("the native bootstrap draws what the irrCAC loop drew", {
  levels <- c("I", "P", "C")
  two <- data.frame(
    human = rep(c("I", "P", "C", "C", "P"), 6),
    judge = rep(c("I", "C", "C", "P", "P"), 6)
  )
  three <- data.frame(
    r1 = rep(c("I", "P", "C", "C"), 5),
    r2 = rep(c("I", NA, "C", "P"), 5),
    r3 = rep(c("P", "P", NA, "C"), 5)
  )
  for (weights in c("unweighted", "ordinal", "quadratic")) {
    gwet <- if (weights == "unweighted") "gwet_ac1" else "gwet_ac2"
    for (ratings in list(two, three)) {
      coefficients <- scenario_coefficients(weights, ncol(ratings))
      chance <- chance_categories(ratings, levels)
      old <- withr::with_seed(3, irrcac_bootstrap(
        ratings, coefficients, 120, weights, levels, chance
      ))
      # A chunk of seven columns at most: chunking must not move a draw.
      new <- withr::with_seed(3, bootstrap_coefficients(
        ratings, coefficients, 120, weights, levels, chance,
        chunk_cells = 7L * nrow(ratings)
      ))
      expect_identical(new, old, info = weights)
      whole <- withr::with_seed(3, bootstrap_coefficients(
        ratings, coefficients, 120, weights, levels, chance
      ))
      expect_identical(whole, old, info = weights)
    }
  }
})

test_that("the bootstrap holds one chunk of resamples at a time", {
  ratings <- data.frame(
    human = rep(c("I", "P", "C"), 10),
    judge = rep(c("I", "C", "C"), 10)
  )
  widths <- integer(0)
  states <- list()
  local_mocked_bindings(
    native_resample_values = function(kernel, ratings, idx, coefficients,
                                      ...) {
      widths <<- c(widths, ncol(idx))
      states[[length(states) + 1L]] <<- get(".Random.seed", globalenv())
      lapply(rlang::set_names(coefficients), function(co) {
        rep(0, ncol(idx))
      })
    }
  )
  levels <- c("I", "P", "C")
  withr::with_seed(8, bootstrap_coefficients(
    ratings, "gwet_ac1", 95, "unweighted", levels, levels,
    chunk_cells = 300L
  ))
  expect_identical(widths, c(rep(10L, 9), 5L))
  # Each chunk is scored as soon as its own resamples are drawn: the stream
  # stands exactly that many draws further on, so no later chunk's indices
  # were drawn ahead of it.
  expected <- withr::with_seed(8, lapply(widths, function(width) {
    for (i in seq_len(width)) sample.int(30L, 30L, replace = TRUE)
    get(".Random.seed", globalenv())
  }))
  expect_identical(states, expected)
})

test_that("kappa's native path is refused for more than two raters", {
  three <- data.frame(r1 = c("a", "b", "a"), r2 = c("a", "b", "b"),
                      r3 = c("b", "b", "a"))
  kernel <- agreement_kernel(three, "unweighted", c("a", "b"), c("a", "b"))
  expect_error(
    agreement_kernel_draws(kernel, matrix(1, 3, 1), "cohen_kappa"),
    "two raters"
  )
})
