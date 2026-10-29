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

test_that("unknown observed labels are named from every entry point", {
  ratings <- data.frame(
    human = c("A", "B", "A", "B"), judge = c("A", "C", "D", "B")
  )
  expect_error(
    agreement(ratings, label_levels = c("A", "B"), n_boot = 10),
    'Observed labels not present in `label_levels`: "C" and "D"',
    fixed = TRUE
  )
  expect_error(
    confusion(ratings, label_levels = c("A", "B", "C")),
    'Observed label not present in `label_levels`: "D"',
    fixed = TRUE
  )
  gold <- data.frame(
    human_label = c("A", "B", "A", "C"), judge_label = c("A", "B", "A", "B")
  )
  expect_error(
    calibrate_judge(gold, label_levels = c("A", "B"), n_boot = 10),
    'Observed label not present in `label_levels`: "C"',
    fixed = TRUE
  )
  expect_error(
    summarize_stability_runs(
      as.matrix(ratings), label_levels = c("A", "B"), n_boot = 10
    ),
    "Observed labels not present"
  )
})

test_that("label universe minimums are stated per entry point", {
  ratings <- data.frame(human = c("A", "B"), judge = c("A", "B"))
  expect_error(
    agreement(ratings, label_levels = character(0), n_boot = 10),
    "at least 1 unique, non-missing label\\."
  )
  gold <- data.frame(human_label = c("A", "B"), judge_label = c("A", "B"))
  expect_error(
    calibrate_judge(gold, label_levels = "A", n_boot = 10),
    "at least 2 unique, non-missing labels\\."
  )
})

test_that("absent explicit columns are pluralised", {
  ratings <- data.frame(human = "A", judge = "B")
  expect_error(
    agreement(ratings, rater_cols = c("human", "x", "y")),
    "names columns not in `x`: x and y"
  )
  expect_error(
    agreement(ratings, rater_cols = c("human", "x")),
    "names column not in `x`: x"
  )
})

test_that("duplicated column names are rejected before selection", {
  ratings <- data.frame(c("A", "B"), c("A", "B"), c("B", "B"))
  names(ratings) <- c("human", "judge", "human")
  expect_error(
    agreement(ratings, rater_cols = c("human", "judge"), n_boot = 10),
    "duplicated column name: human"
  )
  expect_error(agreement(ratings, n_boot = 10), "duplicated column name")
})

test_that("inherited coefficients follow an overridden weights argument", {
  gold <- data.frame(
    human = c("I", "P", "C", "C", "P", "I", "C", "C", "P", "I", "C", "P"),
    judge = c("I", "P", "C", "P", "P", "I", "C", "C", "C", "I", "C", "P")
  )
  ordinal <- calibrate_judge(
    gold, judge_col = "judge", human_cols = "human",
    label_levels = c("I", "P", "C"), scale = "ordinal", n_boot = 30, seed = 1
  )
  nominal <- calibrate_judge(
    gold, judge_col = "judge", human_cols = "human",
    label_levels = c("I", "P", "C"), n_boot = 30, seed = 1
  )
  expect_identical(
    agreement(ordinal, weights = "unweighted")$coefficient,
    c("cohen_kappa", "krippendorff_alpha", "gwet_ac1")
  )
  expect_identical(
    agreement(nominal, weights = "linear")$coefficient,
    c("cohen_kappa", "krippendorff_alpha", "gwet_ac2")
  )
  # An explicit request is still validated against the inherited weights.
  expect_error(
    agreement(ordinal, coefficients = "gwet_ac1"), "AC2, not AC1.*weights"
  )
  expect_error(
    agreement(nominal, coefficients = "gwet_ac2"), "Set `weights`"
  )
})

test_that("count and probability validators reject odd numeric input cleanly", {
  expect_error(check_count(3e9), "must be at most 2147483647")
  expect_error(check_count(2147483648, "n_boot"), "`n_boot` must be at most")
  expect_identical(check_count(2147483647), 2147483647L)
  expect_error(check_count(-3e9), "whole number")
  expect_error(check_count(2.5), "whole number")
  expect_error(check_count(1 + 0i), "whole number")
  expect_error(check_count(list(1)), "whole number")
  expect_error(check_probability(1i), "between 0 and 1")
  expect_error(check_probability(list(0.5)), "between 0 and 1")
  expect_identical(check_count(2), 2L)
})

test_that("no bootstrap is drawn when nothing is estimable", {
  ratings <- data.frame(human = rep("C", 6), judge = rep("C", 6))
  set.seed(4)
  before <- runif(1)
  set.seed(4)
  out <- agreement(ratings, n_boot = 50)
  after <- runif(1)
  expect_true(all(out$status == "insufficient_variation"))
  expect_identical(before, after)

  # Variation is sufficient here, but the only requested coefficient is
  # undefined for three raters, so nothing is estimable and nothing is drawn.
  three <- data.frame(
    r1 = c("C", "C", "P", "P"),
    r2 = c("C", "P", "P", "P"),
    r3 = c("C", "C", "P", "C")
  )
  set.seed(4)
  before <- runif(1)
  set.seed(4)
  out <- agreement(
    three, coefficients = "cohen_kappa", n_boot = 50,
    rater_cols = c("r1", "r2", "r3")
  )
  after <- runif(1)
  expect_identical(out$status, "invalid_rater_count")
  expect_identical(before, after)
  # A coefficient that is estimable still consumes the stream.
  set.seed(4)
  agreement(
    three, coefficients = "gwet_ac1", n_boot = 50,
    rater_cols = c("r1", "r2", "r3")
  )
  expect_false(identical(runif(1), after))
})

test_that("the exact lower bound never exceeds alpha with missing ratings", {
  # Alpha weights each item by its number of ratings, so items rated twice
  # count for less than items rated three times; an unscaled bound on the
  # share of agreeing items overshot the unweighted estimate here
  # (0.175 > 0.101). The quadratic case is extra coverage: it held before.
  pairs <- data.frame(
    r1 = rep(c("a", "b", "c"), length.out = 200),
    r2 = rep(c("a", "b", "c"), length.out = 200),
    r3 = NA_character_
  )
  spread <- data.frame(
    r1 = rep("a", 200), r2 = rep("b", 200), r3 = rep("c", 200)
  )
  d <- rbind(pairs, spread)
  for (weights in c("unweighted", "quadratic")) {
    out <- agreement(
      d, coefficients = "krippendorff_alpha", n_boot = 20, seed = 1,
      label_levels = c("a", "b", "c"), weights = weights,
      rater_cols = c("r1", "r2", "r3")
    )
    expect_identical(out$status, "ok")
    expect_lte(out$conf_low_exact, out$estimate)
  }
})

test_that("a sample with no item rated twice reports without warnings", {
  ratings <- data.frame(human = c("a", NA, "b"), judge = c(NA, "b", NA))
  expect_no_warning(out <- agreement(ratings, n_boot = 10, seed = 1))
  expect_true(all(out$status == "insufficient_items"))
})
