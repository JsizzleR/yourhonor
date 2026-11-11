# acceptance_policy(): its arguments, validation and error classes.

test_that("policy controls validate", {
  expect_error(
    acceptance_policy(.8, 50, 10, .8, 0, NULL, required_probes = "unknown"),
    "Unknown `required_probes`"
  )
  expect_error(
    acceptance_policy(
      .8, 50, 10, .8, 0, NULL, required_probes = c("verbosity", "verbosity"),
      min_probe_n = 10, max_probe_flip_rate = .1,
      max_probe_directional_rate = .1, max_probe_missing_rate = 0
    ),
    "must not repeat"
  )
  expect_error(
    acceptance_policy(.8, 50, 10, .8, 0, NULL, required_probes = NA),
    "character vector of probe names"
  )
  expect_error(
    acceptance_policy(.8, 50, 10, c(I = 1.2), 0, NULL),
    "min_recall"
  )
  expect_error(
    acceptance_policy(.8, 50, 10, .8, 0, NULL, min_precision = c(1, 2)),
    "min_precision"
  )
  expect_error(
    acceptance_policy(2, 50, 10, .8, 0, NULL),
    "min_agreement_lower"
  )
  expect_error(
    acceptance_policy(.8, 50, 10, .8, 0, NULL, min_human_reliability_lower = 3),
    "min_human_reliability_lower"
  )
  expect_error(acceptance_policy(.8, 1, 10, .8, 0, NULL), "min_n")
  expect_error(acceptance_policy(.8, 50, 0, .8, 0, NULL), "min_class_n")
  expect_error(acceptance_policy(.8, 50, 10, .8, 2, NULL), "max_missing_rate")
  expect_error(acceptance_policy(.8, 50, 10, .8, 0, 2), "max_pairwise_disagreement")
  expect_error(
    acceptance_policy(.8, 50, 10, .8, 0, NULL, conf_level = 1), "conf_level"
  )
  expect_error(
    report(make_cal(), policy = list(), output = "md"),
    "acceptance_policy"
  )
})

test_that("required policy arguments cannot be omitted", {
  expect_error(acceptance_policy(), "min_agreement_lower")
  expect_error(acceptance_policy(.8, 50, 10, .8, 0), "max_pairwise_disagreement")
  expect_error(
    acceptance_policy(min_agreement_lower = .8, min_n = 50),
    "min_class_n"
  )
})

test_that("dependent thresholds are required with their gate", {
  expect_error(
    acceptance_policy(.8, 50, 10, .8, 0, max_pairwise_disagreement = 0.1),
    "min_stability_n.*required when.*max_pairwise_disagreement",
    class = "yourhonor_policy_error"
  )
  expect_error(
    acceptance_policy(
      .8, 50, 10, .8, 0, max_pairwise_disagreement = 0.1,
      min_stability_n = 10, min_stability_runs = 2
    ),
    "max_stability_missing_rate.*required when"
  )
  expect_error(
    acceptance_policy(.8, 50, 10, .8, 0, NULL, required_probes = "verbosity"),
    "min_probe_n.*required when.*required_probes",
    class = "yourhonor_policy_error"
  )
  expect_error(
    acceptance_policy(
      .8, 50, 10, .8, 0, NULL, required_probes = "verbosity",
      min_probe_n = 10, max_probe_flip_rate = .1,
      max_probe_directional_rate = .1
    ),
    "max_probe_missing_rate.*required when"
  )
})

test_that("dependent thresholds are rejected without their gate", {
  expect_error(
    acceptance_policy(.8, 50, 10, .8, 0, NULL, max_probe_flip_rate = 0.1),
    "max_probe_flip_rate.*supplied but.*required_probes",
    class = "yourhonor_policy_error"
  )
  expect_error(
    acceptance_policy(.8, 50, 10, .8, 0, NULL, min_probe_n = 10),
    "min_probe_n"
  )
  expect_error(
    acceptance_policy(
      .8, 50, 10, .8, 0, NULL, max_probe_directional_rate = 0.1
    ),
    "max_probe_directional_rate"
  )
  expect_error(
    acceptance_policy(.8, 50, 10, .8, 0, NULL, max_probe_missing_rate = 0),
    "max_probe_missing_rate"
  )
  expect_error(
    acceptance_policy(.8, 50, 10, .8, 0, NULL, min_stability_n = 10),
    "min_stability_n.*supplied but.*max_pairwise_disagreement",
    class = "yourhonor_policy_error"
  )
  expect_error(
    acceptance_policy(.8, 50, 10, .8, 0, NULL, min_stability_runs = 3),
    "min_stability_runs"
  )
  expect_error(
    acceptance_policy(.8, 50, 10, .8, 0, NULL, max_stability_missing_rate = 0),
    "max_stability_missing_rate"
  )
})

test_that("dependent threshold values are validated under their gate", {
  expect_error(
    acceptance_policy(
      .8, 50, 10, .8, 0, 0.1, min_stability_n = 0, min_stability_runs = 3,
      max_stability_missing_rate = 0
    ),
    "min_stability_n"
  )
  expect_error(
    acceptance_policy(
      .8, 50, 10, .8, 0, 0.1, min_stability_n = 10, min_stability_runs = 1,
      max_stability_missing_rate = 0
    ),
    "min_stability_runs"
  )
  expect_error(
    acceptance_policy(
      .8, 50, 10, .8, 0, 0.1, min_stability_n = 10, min_stability_runs = 3,
      max_stability_missing_rate = 2
    ),
    "max_stability_missing_rate"
  )
  expect_error(
    acceptance_policy(
      .8, 50, 10, .8, 0, NULL, required_probes = "verbosity",
      min_probe_n = 0, max_probe_flip_rate = .1,
      max_probe_directional_rate = .1, max_probe_missing_rate = 0
    ),
    "min_probe_n"
  )
  expect_error(
    acceptance_policy(
      .8, 50, 10, .8, 0, NULL, required_probes = "verbosity",
      min_probe_n = 10, max_probe_flip_rate = 2,
      max_probe_directional_rate = .1, max_probe_missing_rate = 0
    ),
    "max_probe_flip_rate"
  )
})

test_that("assessment rows are always one row with scalar numbers", {
  row <- assessment_row("x", "INDETERMINATE", NULL, ">=", 1, "r")
  expect_identical(nrow(row), 1L)
  expect_true(is.na(row$value))
  row <- assessment_row("x", "INDETERMINATE", NA, ">=", NULL, "r")
  expect_true(is.na(row$threshold))
  expect_error(assessment_row("x", "PASS", c(1, 2), ">=", 1, "r"), "single")
})

test_that("orphan thresholds are rejected with a readable sentence", {
  expect_error(
    acceptance_policy(.8, 50, 10, .8, 0, NULL, min_stability_n = 10),
    "`min_stability_n` was supplied but `max_pairwise_disagreement` is NULL\\.",
    class = "yourhonor_policy_error"
  )
  expect_error(
    acceptance_policy(
      .8, 50, 10, .8, 0, NULL, min_probe_n = 10, max_probe_flip_rate = 0.1
    ),
    "were supplied but `required_probes` is empty\\.",
    class = "yourhonor_policy_error"
  )
  expect_error(
    acceptance_policy(.8, 50, 10, .8, 0, NULL, min_probe_n = 10),
    "This threshold is read only when `required_probes` is non-empty"
  )
})

test_that("every acceptance_policy() validation error is classed", {
  bad <- list(
    quote(acceptance_policy(5, 50, 10, .8, 0, NULL)),
    quote(acceptance_policy(.8, 1, 10, .8, 0, NULL)),
    quote(acceptance_policy(.8, 50, 0, .8, 0, NULL)),
    quote(acceptance_policy(.8, 50, 10, 2, 0, NULL)),
    quote(acceptance_policy(.8, 50, 10, .8, 2, NULL)),
    quote(acceptance_policy(.8, 50, 10, .8, 0, 2)),
    quote(acceptance_policy(.8, 50, 10, .8, 0, NULL, min_precision = 2)),
    quote(acceptance_policy(
      .8, 50, 10, .8, 0, NULL, min_human_reliability_lower = 3
    )),
    quote(acceptance_policy(.8, 50, 10, .8, 0, NULL, conf_level = 1)),
    quote(acceptance_policy(.8, 50, 10, .8, 0, NULL, required_probes = "x")),
    quote(acceptance_policy(
      .8, 50, 10, .8, 0, 0.1, min_stability_n = 0, min_stability_runs = 3,
      max_stability_missing_rate = 0
    )),
    quote(acceptance_policy(.8, 50, 10, .8, 0, NULL, coefficient = "bogus")),
    quote(acceptance_policy(.8, 50, 10, .8)),
    quote(acceptance_policy(
      .8, min_class_n = 10, min_recall = .8, max_missing_rate = 0,
      max_pairwise_disagreement = NULL
    ))
  )
  for (call in bad) {
    expect_error(eval(call), class = "yourhonor_policy_error")
  }
  # The rlang argument checks keep their own message and travel as `parent`.
  err <- rlang::catch_cnd(
    acceptance_policy(.8, 50, 10, .8, 0, NULL, coefficient = "bogus")
  )
  expect_match(rlang::cnd_header(err), "`coefficient` must be one of")
  expect_identical(rlang::call_name(err$call), "acceptance_policy")
  expect_s3_class(err$parent, "rlang_error")
  err <- rlang::catch_cnd(acceptance_policy(.8, 50, 10, .8))
  expect_match(rlang::cnd_header(err), "`max_missing_rate` is absent")
  expect_identical(rlang::call_name(err$call), "acceptance_policy")
})

test_that("acceptance_policy() keeps conf_level and coefficient positional", {
  policy <- acceptance_policy(
    0.8, 20, 1, 0, 0.5, NULL, NULL, NULL, NULL, character(0), NULL, NULL,
    NULL, NULL, NULL, NULL, 0.9, "cohen_kappa"
  )
  expect_identical(policy$conf_level, 0.9)
  expect_identical(policy$coefficient, "cohen_kappa")
  expect_null(policy$max_abstention_rate)
  expect_identical(
    names(formals(acceptance_policy))[17:19],
    c("conf_level", "coefficient", "max_abstention_rate")
  )
})

test_that("class thresholds are stored as doubles, keeping their class names", {
  policy <- acceptance_policy(
    min_agreement_lower = 0.5, min_n = 5, min_class_n = 2,
    min_recall = c(I = 1L, P = 0L), max_missing_rate = 0,
    max_pairwise_disagreement = NULL, min_precision = 1L
  )
  expect_identical(policy$min_recall, c(I = 1, P = 0))
  expect_identical(policy$min_precision, 1)
})
