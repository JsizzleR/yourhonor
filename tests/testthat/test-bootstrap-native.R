# The bootstraps score resamples natively; irrCAC, through safe_coeff_point()
# and chance_corrected_stability(), is the oracle every native value must equal.

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
  withr::local_seed(20261111)
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
  # five-decimal values. Which side of it a sum of doubles lands on, and so
  # which way it rounds, depends on the platform's arithmetic (irrCAC's own
  # sums round it up to 0.10938 on macOS arm64 and Windows, and down to
  # 0.10937 on Linux x86-64), so the native kernel cannot vouch for its
  # rounding and irrCAC must score it, whichever way irrCAC rounds.
  ratings <- data.frame(
    r1 = c("a", "a", "a", "c", "b", "c", "a", "a", "a", "c"),
    r2 = c("b", "c", "a", "a", "b", "c", "c", "b", "c", "c")
  )
  levels <- c("a", "b", "c")
  oracle_calls <- 0L
  oracle <- safe_coeff_point
  local_mocked_bindings(safe_coeff_point = function(...) {
    oracle_calls <<- oracle_calls + 1L
    oracle(...)
  })
  expected <- oracle(
    ratings, "krippendorff_alpha", "unweighted", levels, levels
  )
  expect_identical(expected, round(expected, 5))
  expect_true(round(expected * 1e5) %in% c(10937, 10938))
  kernel <- agreement_kernel(ratings, "unweighted", levels, levels)
  raw <- agreement_kernel_draws(
    kernel, matrix(1, 10, 1), "krippendorff_alpha"
  )$krippendorff_alpha
  expect_true(raw$oracle)
  expect_true(round(raw$value * 1e5) %in% c(10937, 10938))
  resample <- function() {
    native_resample_values(
      kernel, ratings, matrix(seq_len(10), ncol = 1), "krippendorff_alpha",
      "unweighted", levels, levels
    )$krippendorff_alpha
  }
  expect_identical(resample(), expected)
  expect_identical(oracle_calls, 1L)
  # Whichever way irrCAC rounds, its value is the one returned, including
  # the side the native sums did not round to.
  for (side in c(0.10937, 0.10938)) {
    local_mocked_bindings(safe_coeff_point = function(...) side)
    expect_identical(resample(), side)
  }
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

# The stability bootstrap as it was: one chance_corrected_stability() call
# per resample, and so one irrCAC call for each resample past its
# availability checks.
irrcac_stability_bootstrap <- function(runs, item_disagreement, n_boot,
                                       conf_level, weights, label_levels,
                                       chance_levels, worst_case,
                                       best_case) {
  n <- nrow(runs)
  pairwise <- numeric(n_boot)
  worst <- numeric(n_boot)
  best <- numeric(n_boot)
  stability <- numeric(n_boot)
  for (i in seq_len(n_boot)) {
    idx <- sample.int(n, n, replace = TRUE)
    pairwise[[i]] <- mean_or_na(item_disagreement[idx])
    worst[[i]] <- mean_or_na(worst_case[idx])
    best[[i]] <- mean_or_na(best_case[idx])
    stability[[i]] <- chance_corrected_stability(
      runs[idx, , drop = FALSE], weights, label_levels, chance_levels
    )$estimate
  }
  list(
    pairwise = widen_to_wilson(
      bootstrap_quantiles(pairwise, conf_level), item_disagreement, conf_level
    ),
    worst_case = widen_to_wilson(
      bootstrap_quantiles(worst, conf_level), worst_case, conf_level
    ),
    best_case = widen_to_wilson(
      bootstrap_quantiles(best, conf_level), best_case, conf_level
    ),
    stability = bootstrap_quantiles(stability, conf_level)
  )
}

# Random runs: items judged in some runs and not others (some with one
# judgment or none), runs missing on most items, and declared levels no run
# used.
random_runs <- function(n_items, n_runs, levels, used, missing) {
  probs <- stats::runif(length(used)) + 0.2
  runs <- matrix(
    sample(used, n_items * n_runs, TRUE, probs), n_items, n_runs
  )
  runs[stats::runif(n_items * n_runs) < missing] <- NA
  sparse <- sample.int(n_runs, 1)
  runs[stats::runif(n_items) < 0.8, sparse] <- NA
  runs
}

test_that("native stability resample values equal chance_corrected_stability()", {
  oracle_calls <- 0L
  oracle <- chance_corrected_stability
  local_mocked_bindings(chance_corrected_stability = function(...) {
    oracle_calls <<- oracle_calls + 1L
    oracle(...)
  })
  withr::local_seed(20261112)
  weight_schemes <- c("unweighted", "linear", "quadratic", "ordinal")
  scored <- 0L
  unscored <- 0L
  few_items <- 0L
  single_category <- 0L
  unused_level_cases <- 0L
  fallbacks <- 0L
  for (case in seq_len(48)) {
    weights <- weight_schemes[[(case - 1L) %% 4L + 1L]]
    n_levels <- sample(2:5, 1)
    levels <- as.character(seq_len(n_levels))
    used <- if (n_levels > 2L && case %% 3L == 0L) {
      sort(sample(levels, n_levels - 1L))
    } else {
      levels
    }
    n_runs <- sample(2:5, 1)
    n_items <- if (case %% 5L == 0L) sample(2:6, 1) else sample(8:60, 1)
    runs <- random_runs(
      n_items, n_runs, levels, used,
      missing = if (case %% 4L < 2L) 0.3 else 0.05
    )
    if (all(is.na(runs))) next
    chance <- chance_categories(runs, levels)
    kernel <- stability_kernel(runs, weights, levels, chance)
    idx <- replicate(40, sample.int(n_items, n_items, TRUE))
    before <- oracle_calls
    native <- stability_resample_values(
      kernel, runs, idx, weights, levels, chance
    )
    fallbacks <- fallbacks + oracle_calls - before
    expected <- vapply(seq_len(ncol(idx)), function(j) {
      oracle(runs[idx[, j], , drop = FALSE], weights, levels, chance)$estimate
    }, numeric(1))
    expect_identical(native, expected, info = paste("case", case, weights))
    usable <- rowSums(!is.na(runs)) >= 2L
    enough <- colSums(matrix(usable[idx], n_items)) >= 2L
    few_items <- few_items + sum(!enough)
    single_category <- single_category + sum(vapply(
      which(enough), function(j) {
        samp <- runs[idx[, j], , drop = FALSE]
        samp <- samp[rowSums(!is.na(samp)) >= 2L, , drop = FALSE]
        length(unique(stats::na.omit(as.vector(samp)))) < 2L
      }, logical(1)
    ))
    if (length(setdiff(levels, runs)) > 0L) {
      unused_level_cases <- unused_level_cases + 1L
    }
    unscored <- unscored + sum(is.na(expected))
    scored <- scored + sum(!is.na(expected))
  }
  # Resamples with fewer than two usable items and with a single category,
  # and runs with declared levels nobody used, were compared, and irrCAC
  # scored only the rare value the native arithmetic could not vouch for
  # (how many depends on the platform's arithmetic; the boundary test below
  # makes it score one on every platform).
  expect_gt(few_items, 0)
  expect_gt(single_category, 0)
  expect_gt(unused_level_cases, 0)
  expect_gte(unscored, few_items + single_category)
  expect_gt(scored, 1000)
  expect_lt(fallbacks / scored, 0.01)
})

test_that("a stability resample on a rounding boundary is scored by irrCAC", {
  # Linear AC2 is 1/64 = 0.015625 here, halfway between two five-decimal
  # values. As for alpha above, which way a sum of doubles rounds it depends
  # on the platform (irrCAC's own sums round it up to 0.01563 on macOS arm64
  # and down to 0.01562 on Linux and Windows x86-64), so irrCAC must score
  # it, whichever way it rounds.
  runs <- matrix(
    c("b", "a", "b", "b", "c", "b", "b", "c", "b", "c",
      "c", "c", "b", "a", "a"),
    nrow = 5
  )
  levels <- c("a", "b", "c")
  oracle_calls <- 0L
  oracle <- chance_corrected_stability
  local_mocked_bindings(chance_corrected_stability = function(...) {
    oracle_calls <<- oracle_calls + 1L
    oracle(...)
  })
  expected <- oracle(runs, "linear", levels, levels)
  expect_identical(expected$estimate, round(expected$estimate, 5))
  expect_true(round(expected$estimate * 1e5) %in% c(1562, 1563))
  kernel <- stability_kernel(runs, "linear", levels, levels)
  raw <- agreement_kernel_draws(kernel, matrix(1, 5, 1), "gwet_ac1")$gwet_ac1
  expect_true(raw$oracle)
  expect_true(round(raw$value * 1e5) %in% c(1562, 1563))
  resample <- function() {
    stability_resample_values(
      kernel, runs, matrix(seq_len(5), ncol = 1), "linear", levels, levels
    )
  }
  expect_identical(resample(), expected$estimate)
  expect_identical(oracle_calls, 1L)
  # Whichever way irrCAC rounds, its value is the one returned, including
  # the side the native sums did not round to.
  for (side in c(0.01562, 0.01563)) {
    local_mocked_bindings(
      chance_corrected_stability = function(...) list(estimate = side)
    )
    expect_identical(resample(), side)
  }
})

test_that("the native stability bootstrap draws what the irrCAC loop drew", {
  levels <- c("I", "P", "C")
  complete <- cbind(
    rep(c("I", "P", "C", "C", "P"), 6),
    rep(c("I", "C", "C", "P", "P"), 6),
    rep(c("I", "C", "P", "P", "P"), 6)
  )
  holes <- cbind(
    rep(c("I", "P", "C", "C", NA), 5),
    rep(c("I", NA, "C", "P", NA), 5),
    rep(c("P", "P", NA, "C", "I"), 5)
  )
  # One usable item: every resample is insufficient_items or draws it twice.
  lone <- cbind(c("I", NA, NA, "C"), c("P", "C", NA, NA))
  for (weights in c("unweighted", "ordinal", "quadratic")) {
    for (runs in list(complete, holes, lone)) {
      d <- item_pairwise_disagreement(runs)
      wc <- item_worst_case_disagreement(runs, d)
      bc <- item_best_case_disagreement(runs)
      chance <- chance_categories(runs, levels)
      old <- withr::with_seed(3, irrcac_stability_bootstrap(
        runs, d, 120, 0.9, weights, levels, chance, wc, bc
      ))
      # A chunk of seven columns at most: chunking must not move a draw.
      new <- withr::with_seed(3, bootstrap_stability(
        runs, d, 120, 0.9, weights, levels, chance, wc, bc,
        chunk_cells = 7L * nrow(runs)
      ))
      expect_identical(new, old, info = weights)
      whole <- withr::with_seed(3, bootstrap_stability(
        runs, d, 120, 0.9, weights, levels, chance, wc, bc
      ))
      expect_identical(whole, old, info = weights)
    }
  }
})

test_that("the stability bootstrap holds one chunk of resamples at a time", {
  runs <- cbind(rep(c("I", "P", "C"), 10), rep(c("I", "C", "C"), 10))
  widths <- integer(0)
  states <- list()
  local_mocked_bindings(
    stability_resample_values = function(kernel, runs, idx, ...) {
      widths <<- c(widths, ncol(idx))
      states[[length(states) + 1L]] <<- get(".Random.seed", globalenv())
      rep(0, ncol(idx))
    }
  )
  levels <- c("I", "P", "C")
  d <- item_pairwise_disagreement(runs)
  withr::with_seed(8, bootstrap_stability(
    runs, d, 95, 0.95, "unweighted", levels, levels, chunk_cells = 300L
  ))
  expect_identical(widths, c(rep(10L, 9), 5L))
  expected <- withr::with_seed(8, lapply(widths, function(width) {
    for (i in seq_len(width)) sample.int(30L, 30L, replace = TRUE)
    get(".Random.seed", globalenv())
  }))
  expect_identical(states, expected)
})
