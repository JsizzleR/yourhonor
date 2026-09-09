make_gold <- function() {
  tibble::tibble(
    id = paste0("q", 1:8),
    input = paste("answer", 1:8),
    target = "is it correct?",
    human_label = c("C", "C", "P", "I", "C", "P", "I", "C"),
    judge_label = c("C", "C", "C", "I", "C", "P", "P", "C")
  )
}

test_that("multiple human raters produce reliability and per-rater evidence", {
  gold <- tibble::tibble(
    input = paste("answer", 1:8),
    h1 = c("C", "C", "P", "I", "C", "P", "I", "C"),
    h2 = c("C", "P", "P", "I", "C", "C", "I", "C"),
    h3 = c("C", "C", "P", "P", "C", "P", "I", "C"),
    judge_label = c("C", "C", "C", "I", "C", "P", "P", "C")
  )
  cal <- calibrate_judge(
    gold, human_cols = c("h1", "h2", "h3"),
    label_levels = c("I", "P", "C"), n_boot = 100, seed = 1
  )
  expect_false(is.null(cal$human_reliability))
  expect_identical(cal$human_ceiling, cal$human_reliability)
  expect_setequal(
    cal$human_reliability$coefficient,
    c("krippendorff_alpha", "gwet_ac1")
  )
  expect_setequal(unique(cal$per_rater_agreement$rater), c("h1", "h2", "h3"))
  expect_true(all(c("human_consensus", "consensus_tie") %in% names(cal$items)))
})

test_that("multi-rater consensus requires at least two observed humans", {
  gold <- tibble::tibble(
    h1 = rep(c("I", "C"), 30),
    h2 = NA_character_,
    h3 = NA_character_,
    judge_label = rep(c("I", "C"), 30)
  )
  expect_error(
    calibrate_judge(
      gold, human_cols = c("h1", "h2", "h3"),
      label_levels = c("I", "C"), n_boot = 50, seed = 1
    ),
    "complete judge-human pairs"
  )
})

test_that("single human rater has no reliability benchmark", {
  cal <- calibrate_judge(
    make_gold(), label_levels = c("I", "P", "C"), n_boot = 50, seed = 1
  )
  expect_null(cal$human_reliability)
  expect_null(cal$human_ceiling)
})

test_that("missing human columns and duplicate IDs error", {
  expect_error(
    calibrate_judge(make_gold(), human_cols = c("human_label", "nope")),
    "missing human rater"
  )
  gold <- make_gold()
  gold$id[[2]] <- gold$id[[1]]
  expect_error(calibrate_judge(gold, id_col = "id"), "unique")
})

test_that("pre-graded labels create a complete audit object", {
  cal <- calibrate_judge(
    make_gold(), id_col = "id", label_levels = c("I", "P", "C"),
    n_boot = 100, seed = 1
  )
  expect_s3_class(cal, "yourhonor_calibration")
  expect_equal(cal$n_items, 8)
  expect_equal(cal$n_input, 8)
  expect_identical(cal$label_levels, c("I", "P", "C"))
  expect_true(all(c("support", "recall_conf_low") %in% names(cal$confusion_metrics)))
  expect_equal(cal$provenance$id_col, "id")
})

test_that("judge functions receive only IDs and declared inputs", {
  gold <- make_gold()
  gold$id <- paste0("human-label-", gold$human_label, "-", seq_len(nrow(gold)))
  gold$judge_label <- NULL
  seen <- NULL
  seen_ids <- NULL
  seen_input <- NULL
  judge <- function(d) {
    seen <<- names(d)
    seen_ids <<- d$.item_id
    seen_input <<- d$input
    rep("C", nrow(d))
  }
  cal <- calibrate_judge(
    gold, judge, input_cols = c("input", "target"), id_col = "id",
    label_levels = c("I", "P", "C"), n_boot = 50, seed = 1
  )
  expect_identical(seen, c(".item_id", "input", "target"))
  expect_identical(seen_ids, seq_len(nrow(gold)))
  expect_false(identical(seen_input, gold$input))
  expect_false("human_label" %in% seen)
  expect_false(any(gold$id %in% seen_ids))
  expect_true(all(cal$items$judge_label == "C"))
})

test_that("judge input must be explicit and cannot expose labels", {
  gold <- make_gold()
  gold$judge_label <- NULL
  judge <- function(d) rep("C", nrow(d))
  expect_error(calibrate_judge(gold, judge), "input_cols")
  expect_error(
    calibrate_judge(gold, judge, input_cols = c("input", "human_label")),
    "Protected"
  )
  expect_error(
    calibrate_judge(gold, judge, input_cols = c("input", "input")),
    "unique"
  )
  expect_error(
    calibrate_judge(make_gold(), human_cols = "human_label",
                    judge_col = "human_label"),
    "must differ"
  )
})

test_that("structured judge output is keyed and reordered safely", {
  gold <- make_gold()
  gold$judge_label <- NULL
  judge <- function(d) {
    item <- as.integer(sub("answer ", "", d$input, fixed = TRUE))
    out <- tibble::tibble(
      .item_id = d$.item_id,
      label = ifelse(item %% 2L == 1L, "C", "P")
    )
    out[nrow(out):1, ]
  }
  cal <- calibrate_judge(
    gold, judge, input_cols = "input", id_col = "id",
    label_levels = c("I", "P", "C"), n_boot = 50, seed = 1
  )
  expect_identical(cal$items$judge_label, rep(c("C", "P"), 4))
})

test_that("blind grading order is seeded and does not leak RNG state", {
  gold <- make_gold()
  gold$judge_label <- NULL
  orders <- list()
  judge <- function(d) {
    orders[[length(orders) + 1L]] <<- d$input
    rep("C", nrow(d))
  }
  run <- function() calibrate_judge(
    gold, judge, input_cols = "input", label_levels = c("I", "P", "C"),
    n_boot = 50, seed = 17
  )
  invisible(run())
  first <- orders[[1]]
  invisible(run())
  expect_identical(orders[[2]], first)

  set.seed(123)
  expected <- runif(1)
  set.seed(123)
  invisible(run())
  actual <- runif(1)
  expect_equal(actual, expected)
})

test_that("unknown labels and malformed judge output are rejected", {
  gold <- make_gold()
  gold$judge_label <- NULL
  expect_error(
    calibrate_judge(
      gold, function(d) rep("garbage", nrow(d)), input_cols = "input",
      label_levels = c("I", "P", "C")
    ),
    "outside"
  )
  expect_error(
    calibrate_judge(
      gold, function(d) rep("C", 1), input_cols = "input",
      label_levels = c("I", "P", "C")
    ),
    "return one label per row"
  )
})

test_that("missing judge labels and consensus ties remain visible", {
  gold <- make_gold()
  gold$judge_label[[2]] <- NA
  expect_warning(
    cal <- calibrate_judge(
      gold, label_levels = c("I", "P", "C"), n_boot = 50, seed = 1
    ),
    "lacked"
  )
  expect_equal(cal$n_items, 7)
  expect_equal(cal$n_excluded, 1)

  two <- tibble::tibble(
    h1 = c("C", "C", "P", "P"),
    h2 = c("C", "P", "P", "C"),
    judge_label = c("C", "P", "P", "C")
  )
  expect_warning(
    tied <- calibrate_judge(
      two, human_cols = c("h1", "h2"), label_levels = c("P", "C"),
      n_boot = 50, seed = 1
    ),
    "lacked"
  )
  expect_equal(tied$n_consensus_ties, 2)
  expect_equal(tied$n_excluded, 2)
})

test_that("agreement can reuse or explicitly recompute a calibration", {
  cal <- calibrate_judge(
    make_gold(), label_levels = c("I", "P", "C"), n_boot = 50, seed = 1
  )
  expect_identical(agreement(cal), cal$agreement)
  one <- agreement(cal, coefficients = "cohen_kappa", n_boot = 50, seed = 1)
  expect_equal(one$coefficient, "cohen_kappa")
})

test_that("ordinal calibration requires declared label order", {
  expect_error(calibrate_judge(make_gold(), scale = "ordinal"), "label_levels")
  cal <- calibrate_judge(
    make_gold(), scale = "ordinal", label_levels = c("I", "P", "C"),
    n_boot = 50, seed = 1
  )
  expect_equal(cal$weights, "ordinal")
  expect_true("gwet_ac2" %in% cal$agreement$coefficient)
  expect_false("gwet_ac1" %in% cal$agreement$coefficient)
})

test_that("a blank judge label is rejected on the first call and named as such", {
  gold <- tibble::tibble(
    human_label = c("C", "C", "P", "I", "C", "P"),
    input = paste("answer", 1:6)
  )
  calls <- 0L
  blank_judge <- function(d) {
    calls <<- calls + 1L
    labels <- rep("C", nrow(d))
    labels[[1]] <- ""
    labels
  }
  expect_error(
    test_retest(gold, blank_judge, input_cols = "input", n_runs = 3, n_boot = 10, seed = 1),
    "`judge` contains empty"
  )
  expect_equal(calls, 1L)
  expect_error(
    calibrate_judge(gold, blank_judge, input_cols = "input", n_boot = 10, seed = 1),
    "`judge` contains empty"
  )
})

test_that("element names and attributes never reach the judge", {
  gold <- tibble::tibble(
    human_label = c("C", "C", "P", "I", "C", "P", "I", "C"),
    input = paste("answer", 1:8)
  )
  gold$input <- stats::setNames(gold$input, gold$human_label)
  attr(gold$input, "secret") <- gold$human_label
  attr(gold, "comment") <- "human labels: C C P I C P I C"
  seen <- NULL
  judge <- function(d) {
    seen <<- list(
      names = names(d$input),
      column_attrs = names(attributes(d$input)),
      table_attrs = names(attributes(d))
    )
    rep("C", nrow(d))
  }
  calibrate_judge(
    gold, judge, input_cols = "input", label_levels = c("I", "P", "C"),
    n_boot = 10, seed = 1
  )
  expect_null(seen$names)
  expect_null(seen$column_attrs)
  expect_setequal(seen$table_attrs, c("names", "row.names", "class"))

  # List columns are sanitised element by element.
  gold$input <- lapply(seq_len(8), function(i) {
    structure(list(text = paste("answer", i)), secret = gold$human_label[[i]])
  })
  seen_list <- NULL
  list_judge <- function(d) {
    seen_list <<- lapply(d$input, function(el) names(attributes(el)))
    rep("C", nrow(d))
  }
  calibrate_judge(
    gold, list_judge, input_cols = "input", label_levels = c("I", "P", "C"),
    n_boot = 10, seed = 1
  )
  expect_true(all(vapply(seen_list, identical, logical(1), "names")))

  # Classed list columns (list_of, AsIs) are traversed too; S4 objects pass
  # through whole.
  setClass("yourhonor_blob", representation(payload = "character"))
  gold$as_is <- I(lapply(gold$human_label, function(g) structure(1, hidden = g)))
  gold$list_of <- vctrs::list_of(!!!lapply(gold$human_label, function(g) stats::setNames(1, g)))
  gold$blob <- lapply(gold$human_label, function(g) new("yourhonor_blob", payload = "static"))
  gold$when <- lapply(gold$human_label, function(g) {
    stats::setNames(as.POSIXlt("2024-01-01", tz = "UTC"), g)
  })
  seen_classed <- NULL
  classed_judge <- function(d) {
    seen_classed <<- list(
      as_is = unlist(lapply(d$as_is, function(el) attr(el, "hidden"))),
      list_of = unlist(lapply(d$list_of, names)),
      blob = vapply(d$blob, function(el) el@payload, character(1)),
      list_of_class = class(d$list_of),
      when = unlist(lapply(d$when, names))
    )
    rep("C", nrow(d))
  }
  calibrate_judge(
    gold, classed_judge, input_cols = c("as_is", "list_of", "blob", "when"),
    label_levels = c("I", "P", "C"), n_boot = 10, seed = 1
  )
  expect_null(seen_classed$as_is)
  expect_null(seen_classed$list_of)
  expect_identical(seen_classed$blob, rep("static", 8))
  expect_true("vctrs_list_of" %in% seen_classed$list_of_class)
  expect_null(seen_classed$when)
})

test_that("sanitising judge input keeps classed columns intact", {
  gold <- tibble::tibble(
    human_label = c("C", "P", "I", "C"),
    when = as.Date("2024-01-01") + 0:3,
    stamp = as.POSIXct("2024-01-01 12:00:00", tz = "Asia/Tokyo") + 0:3,
    local = stats::setNames(
      as.POSIXlt(as.POSIXct("2024-01-01 12:00:00", tz = "UTC") + 0:3),
      c("C", "P", "I", "C")
    ),
    grade = factor(c("x", "y", "x", "y")),
    record = vctrs::new_rcrd(
      list(lo = 1:4, hi = stats::setNames(5:8, c("C", "P", "I", "C"))),
      secret = c("C", "P", "I", "C"), precision = "day", class = "yourhonor_span"
    ),
    nested = tibble::tibble(a = 1:4, a = 5:8, .name_repair = "minimal")
  )
  judge <- function(d) {
    expect_s3_class(d$when, "Date")
    expect_identical(attr(d$stamp, "tzone"), "Asia/Tokyo")
    expect_s3_class(d$local, "POSIXlt")
    expect_null(names(d$local))
    expect_setequal(format(d$local, "%H:%M:%S"), c("12:00:00", "12:00:01", "12:00:02", "12:00:03"))
    expect_s3_class(d$grade, "factor")
    expect_identical(levels(d$grade), c("x", "y"))
    expect_setequal(vctrs::field(d$record, "hi"), 5:8)
    expect_null(names(vctrs::field(d$record, "hi")))
    expect_null(attr(d$record, "secret"))
    expect_identical(attr(d$record, "precision"), "day")
    expect_identical(names(d$nested), c("a", "a"))
    rep("C", nrow(d))
  }
  cal <- calibrate_judge(
    gold, judge, input_cols = c("when", "stamp", "local", "grade", "record", "nested"),
    label_levels = c("I", "P", "C"), n_boot = 10, seed = 1
  )
  expect_equal(cal$n_items, 4)
})
