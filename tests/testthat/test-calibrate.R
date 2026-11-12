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
  expect_false("human_ceiling" %in% names(cal))
  expect_setequal(
    cal$human_reliability$coefficient,
    c("krippendorff_alpha", "gwet_ac1")
  )
  expect_setequal(unique(cal$per_rater_agreement$rater), c("h1", "h2", "h3"))
  expect_all(c("human_consensus", "consensus_tie") %in% names(cal$items))
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
    class = "yourhonor_input_error"
  )
})

test_that("items with one observed human rating are excluded and counted", {
  gold <- tibble::tibble(
    h1 = c("C", "C", "P", "I", "C", "P", "I", "C", "C", "P"),
    h2 = c("C", "C", "P", "I", "C", "P", "I", "C", NA, NA),
    h3 = c("C", "C", "P", "I", "C", "P", "I", "C", NA, NA),
    judge_label = c("C", "C", "P", "I", "C", "P", "I", "C", "C", "P")
  )
  expect_warning(
    cal <- calibrate_judge(
      gold, human_cols = c("h1", "h2", "h3"),
      label_levels = c("I", "P", "C"), n_boot = 20, seed = 1
    ),
    "2 items lacked"
  )
  expect_equal(cal$n_input, 10)
  expect_equal(cal$n_items, 8)
  expect_equal(cal$n_excluded, 2)
  expect_equal(cal$n_insufficient_human_ratings, 2)
  expect_equal(cal$n_consensus_ties, 0)
  tail_items <- cal$items[9:10, ]
  expect_all(is.na(tail_items$human_consensus))
  expect_equal(tail_items$n_human_observed, c(1, 1))
  expect_none(tail_items$consensus_tie)
  expect_all(is.na(tail_items$agree))
  # Per-rater agreement still scores every item against h1.
  h1 <- cal$per_rater_agreement[cal$per_rater_agreement$rater == "h1", ]
  expect_equal(unique(h1$n_items), 10L)
})

test_that("single human rater has no reliability benchmark", {
  cal <- calibrate_judge(
    make_gold(), label_levels = c("I", "P", "C"), n_boot = 50, seed = 1
  )
  expect_null(cal$human_reliability)
  expect_false("human_ceiling" %in% names(cal))
})

test_that("missing human columns and duplicate IDs error", {
  expect_error(
    calibrate_judge(make_gold(), human_cols = c("human_label", "nope")),
    class = "yourhonor_input_error"
  )
  gold <- make_gold()
  gold$id[[2]] <- gold$id[[1]]
  expect_error(
    calibrate_judge(gold, id_col = "id"),
    class = "yourhonor_input_error"
  )
  expect_error(
    calibrate_judge(make_gold(), id_col = "nope"), "id_col",
    class = "yourhonor_input_error"
  )
})

test_that("reserved keys, provenance, judge_col, and judge are validated", {
  gold <- make_gold()
  gold$.item_id <- seq_len(nrow(gold))
  expect_error(calibrate_judge(gold), class = "yourhonor_input_error")

  expect_error(
    calibrate_judge(make_gold(), provenance = "model"),
    "provenance", class = "yourhonor_input_error"
  )
  expect_error(
    calibrate_judge(make_gold(), provenance = list("unnamed")),
    "provenance", class = "yourhonor_input_error"
  )
  expect_error(
    calibrate_judge(make_gold(), provenance = data.frame(a = 1)),
    class = "yourhonor_input_error"
  )
  expect_error(
    calibrate_judge(make_gold(), judge_col = c("a", "b")), "judge_col",
    class = "yourhonor_input_error"
  )
  expect_error(
    calibrate_judge(make_gold(), judge_col = ""), "judge_col",
    class = "yourhonor_input_error"
  )
  expect_error(
    calibrate_judge(make_gold(), judge_col = "nope"), "nope",
    class = "yourhonor_input_error"
  )
  expect_error(
    calibrate_judge(
      make_gold(), judge = "not a function", input_cols = "input"
    ),
    class = "yourhonor_input_error"
  )
  err <- rlang::catch_cnd(calibrate_judge(make_gold(), n_boot = 0))
  expect_identical(rlang::call_name(err$call), "calibrate_judge")
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
  expect_all(c("support", "recall_conf_low") %in% names(cal$confusion_metrics))
  expect_equal(cal$provenance$id_col, "id")
  expect_null(cal$judge_output)
  expect_null(cal$blinding_map)
  expect_all(is.na(cal$items$.judge_item_id))
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
  expect_none(gold$id %in% seen_ids)
  expect_all(cal$items$judge_label == "C")
})

test_that("the blinding map ties judge-side keys back to source rows", {
  gold <- make_gold()
  gold$judge_label <- NULL
  seen <- NULL
  judge <- function(d) {
    seen <<- d
    item <- as.integer(sub("answer ", "", d$input, fixed = TRUE))
    tibble::tibble(
      .item_id = d$.item_id,
      label = ifelse(item %% 2L == 1L, "C", "P"),
      rationale = paste("because", item)
    )
  }
  cal <- calibrate_judge(
    gold, judge, input_cols = "input", id_col = "id",
    label_levels = c("I", "P", "C"), n_boot = 20, seed = 1
  )
  expect_identical(cal$items$.item_id, 1:8)
  expect_setequal(cal$items$.judge_item_id, 1:8)
  expect_false(identical(cal$items$.judge_item_id, cal$items$.item_id))
  # The judge saw each source row under its .judge_item_id.
  expect_identical(
    seen$input[match(cal$items$.judge_item_id, seen$.item_id)],
    gold$input
  )
  map <- cal$blinding_map
  expect_named(map, c(".judge_item_id", ".item_id"))
  expect_identical(
    map$.item_id[match(cal$items$.judge_item_id, map$.judge_item_id)],
    cal$items$.item_id
  )
  # judge_output keeps the judge's own rows and order, plus the source key.
  output <- cal$judge_output
  expect_identical(names(output)[1:2], c(".judge_item_id", ".item_id"))
  expect_identical(output$.judge_item_id, seen$.item_id)
  expect_identical(
    output$rationale, paste("because", sub("answer ", "", seen$input))
  )
  joined <- output[match(cal$items$.item_id, output$.item_id), ]
  expect_identical(joined$label, cal$items$judge_label)
})

test_that("judge input must be explicit and cannot expose labels", {
  gold <- make_gold()
  gold$judge_label <- NULL
  judge <- function(d) rep("C", nrow(d))
  expect_error(
    calibrate_judge(gold, judge), "input_cols",
    class = "yourhonor_input_error"
  )
  expect_error(
    calibrate_judge(gold, judge, input_cols = c("input", "human_label")),
    class = "yourhonor_input_error"
  )
  expect_error(
    calibrate_judge(gold, judge, input_cols = c("input", "input")),
    class = "yourhonor_input_error"
  )
  expect_error(
    calibrate_judge(gold, judge, input_cols = c("input", "nope")),
    "nope", class = "yourhonor_input_error"
  )
  expect_error(
    calibrate_judge(make_gold(), human_cols = "human_label",
                    judge_col = "human_label"),
    class = "yourhonor_input_error"
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

  withr::local_seed(123)
  expected <- runif(1)
  withr::local_seed(123)
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
    class = "yourhonor_judge_error"
  )
  expect_error(
    calibrate_judge(
      gold, function(d) rep("C", 1), input_cols = "input",
      label_levels = c("I", "P", "C")
    ),
    class = "yourhonor_judge_error"
  )
  err <- rlang::catch_cnd(
    calibrate_judge(
      gold, function(d) rep("garbage", nrow(d)), input_cols = "input",
      label_levels = c("I", "P", "C")
    )
  )
  expect_identical(rlang::call_name(err$call), "calibrate_judge")
  precomputed <- make_gold()
  precomputed$judge_label[[1]] <- "garbage"
  expect_error(
    calibrate_judge(precomputed, label_levels = c("I", "P", "C")),
    "judge_label.*outside.*label_levels", class = "yourhonor_input_error"
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
  expect_error(
    calibrate_judge(make_gold(), scale = "ordinal"), "label_levels",
    class = "yourhonor_input_error"
  )
  cal <- calibrate_judge(
    make_gold(), scale = "ordinal", label_levels = c("I", "P", "C"),
    n_boot = 50, seed = 1
  )
  expect_equal(cal$weights, "ordinal")
  expect_true("gwet_ac2" %in% cal$agreement$coefficient)
  expect_false("gwet_ac1" %in% cal$agreement$coefficient)
  expect_error(
    calibrate_judge(make_gold(), weights = "ordinal"),
    "scale = 'ordinal'", class = "yourhonor_input_error"
  )
})

test_that("coefficients = NULL selects the weighted default automatically", {
  explicit_null <- calibrate_judge(
    make_gold(), scale = "ordinal", label_levels = c("I", "P", "C"),
    coefficients = NULL, n_boot = 30, seed = 1
  )
  expect_setequal(
    explicit_null$coefficients,
    c("cohen_kappa", "krippendorff_alpha", "gwet_ac2")
  )
  expect_error(
    calibrate_judge(
      make_gold(), scale = "ordinal", label_levels = c("I", "P", "C"),
      coefficients = c("cohen_kappa", "gwet_ac1"), n_boot = 30
    ),
    "AC2", class = "yourhonor_input_error"
  )
  err <- rlang::catch_cnd(
    calibrate_judge(make_gold(), coefficients = "nope", n_boot = 30)
  )
  expect_identical(rlang::call_name(err$call), "calibrate_judge")
})

test_that("calibration printing shows intervals and interpretations", {
  cal <- calibrate_judge(
    make_gold(), label_levels = c("I", "P", "C"), n_boot = 30, seed = 1
  )
  expect_snapshot(print(cal))

  gold <- tibble::tibble(
    input = paste("answer", 1:8),
    h1 = c("C", "C", "P", "I", "C", "P", "I", "C"),
    h2 = c("C", "P", "P", "I", "C", "C", "I", NA),
    h3 = c("C", "C", "P", "P", "C", "P", "I", NA),
    judge_label = c("C", "C", "C", "I", "C", "P", "P", "C")
  )
  expect_warning(
    multi <- calibrate_judge(
      gold, human_cols = c("h1", "h2", "h3"),
      label_levels = c("I", "P", "C"), n_boot = 30, seed = 1
    ),
    "lacked"
  )
  expect_snapshot(print(multi))
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
    "`judge` contains empty", class = "yourhonor_judge_error"
  )
  expect_equal(calls, 1L)
  expect_error(
    calibrate_judge(gold, blank_judge, input_cols = "input", n_boot = 10, seed = 1),
    "`judge` contains empty", class = "yourhonor_judge_error"
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
  expect_all(vapply(seen_list, identical, logical(1), "names"))

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

test_that("provenance keeps NULL adapter fields and one timestamp format", {
  gold <- make_gold()
  cal <- calibrate_judge(
    gold, label_levels = c("I", "P", "C"), n_boot = 20, seed = 1,
    provenance = list(model = "m")
  )
  expect_all(c("id_col", "audit_id") %in% names(cal$provenance))
  expect_null(cal$provenance$id_col)
  expect_null(cal$provenance$audit_id)
  expect_identical(cal$provenance$model, "m")
  expect_match(
    cal$provenance$created_at, "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$"
  )
  expect_error(
    calibrate_judge(gold, n_boot = 20, provenance = list(a = 1, a = 2)),
    class = "yourhonor_input_error"
  )
  expect_error(
    calibrate_judge(gold, n_boot = 20, provenance = list(a = 1, 2)),
    class = "yourhonor_input_error"
  )
  expect_error(
    calibrate_judge(gold, n_boot = 20, provenance = list(" " = 1)),
    class = "yourhonor_input_error"
  )
})

test_that("structured output is character and cannot carry the judge key", {
  gold <- make_gold()
  gold$judge_label <- NULL
  factor_judge <- function(d) {
    tibble::tibble(
      .item_id = d$.item_id,
      label = factor(rep(c("C", "P"), length.out = nrow(d)))
    )
  }
  cal <- calibrate_judge(
    gold, factor_judge, input_cols = "input", label_levels = c("I", "P", "C"),
    n_boot = 20, seed = 1
  )
  expect_type(cal$judge_output$label, "character")
  output <- cal$judge_output
  joined <- output[match(cal$items$.item_id, output$.item_id), ]
  expect_identical(joined$label, cal$items$judge_label)

  colliding <- function(d) {
    tibble::tibble(.item_id = d$.item_id, .judge_item_id = 0L, label = "C")
  }
  expect_error(
    calibrate_judge(
      gold, colliding, input_cols = "input", label_levels = c("I", "P", "C"),
      n_boot = 20, seed = 1
    ),
    "reserved column .judge_item_id", class = "yourhonor_judge_error"
  )
  graded <- grade_blind(function(d) rep("C", nrow(d)), gold["input"], "C")
  expect_named(
    graded, c("label", "label_levels", "visible_output", "blinding_map")
  )
})

test_that("a degenerate interval prints as degenerate with the exact bound", {
  gold <- tibble::tibble(
    human_label = rep(c("a", "b"), 10),
    judge_label = rep(c("a", "b"), 10)
  )
  cal <- calibrate_judge(gold, n_boot = 200, seed = 1)
  expect_all(cal$agreement$boundary_degenerate)
  # Whitespace is normalised because cli wraps at the console width.
  out <- gsub("\\s+", " ", paste(cli::cli_fmt(print(cal)), collapse = " "))
  exact <- fmt_num(cal$agreement$conf_low_exact[[1]])
  expect_no_match(out, "[1.00, 1.00]", fixed = TRUE)
  expect_match(
    out,
    paste0("cohen_kappa: 1.00 [degenerate; exact lower bound ", exact,
           "] (almost perfect)"),
    fixed = TRUE
  )
})

test_that("gold columns the result writes are reserved", {
  base <- tibble::tibble(
    human_label = c("a", "b", "a", "b"),
    judge_label = c("a", "b", "b", "b")
  )
  for (col in c("agree", ".judge_item_id")) {
    gold <- base
    gold[[col]] <- 1:4
    expect_error(
      calibrate_judge(gold, n_boot = 20, seed = 1),
      paste0(col, ".*reserved"), class = "yourhonor_input_error",
      info = col
    )
  }
  multi <- tibble::tibble(
    h1 = c("a", "b", "a", "b"), h2 = c("a", "b", "a", "b"),
    judge_label = c("a", "b", "b", "b")
  )
  for (col in c("human_consensus", "n_human_observed", "consensus_tie")) {
    gold <- multi
    gold[[col]] <- 1:4
    expect_error(
      calibrate_judge(gold, human_cols = c("h1", "h2"), n_boot = 20, seed = 1),
      paste0(col, ".*reserved"), class = "yourhonor_input_error",
      info = col
    )
    # One rater writes no consensus columns, so the name is free there.
    single <- base
    single[[col]] <- 1:4
    cal <- calibrate_judge(single, n_boot = 20, seed = 1)
    expect_identical(cal$items[[col]], 1:4, info = col)
  }
  # A live judge's labels are written under `judge_col`, so it is reserved too.
  live <- tibble::tibble(x = letters[1:4], human_label = c("a", "b", "a", "b"))
  expect_error(
    calibrate_judge(
      live, judge = function(d) rep("a", nrow(d)), input_cols = "x",
      judge_col = "agree", n_boot = 20, seed = 1
    ),
    "`judge_col`.*reserved", class = "yourhonor_input_error"
  )
})

test_that("an unstructured judge must return an atomic vector without dim", {
  gold <- tibble::tibble(x = letters[1:4], human_label = c("a", "b", "a", "b"))
  for (judge in list(
    function(d) matrix(c("a", "b", "a", "b"), 2, 2),
    function(d) list(list("a"), list("b"), list("a"), list("b")),
    function(d) list("a", "b", "a", "b"),
    function(d) array(c("a", "b", "a", "b"))
  )) {
    expect_error(
      calibrate_judge(gold, judge = judge, input_cols = "x", n_boot = 20),
      class = "yourhonor_judge_error"
    )
  }
  cal <- calibrate_judge(
    gold, judge = function(d) c("a", "b", "a", "b")[d$.item_id],
    input_cols = "x", n_boot = 20, seed = 1
  )
  expect_identical(cal$n_items, 4L)
})

test_that("input_cols is refused without a judge function", {
  expect_error(
    calibrate_judge(make_gold(), input_cols = "input", n_boot = 20),
    "input_cols.*judge = NULL", class = "yourhonor_input_error"
  )
  expect_error(
    calibrate_judge(make_gold(), input_cols = "nonexistent", n_boot = 20),
    "input_cols.*judge = NULL", class = "yourhonor_input_error"
  )
  cal <- calibrate_judge(make_gold(), n_boot = 20, seed = 1)
  expect_null(cal$input_cols)
  expect_identical(cal$provenance$input_cols, character(0))
})

test_that("a structured judge's label column must be atomic without dim", {
  gold <- tibble::tibble(x = letters[1:4], human_label = c("a", "b", "a", "b"))
  judge <- function(d) {
    out <- tibble::tibble(.item_id = d$.item_id)
    out$label <- lapply(c("a", "b", "a", "b"), list)
    out
  }
  expect_error(
    calibrate_judge(gold, judge = judge, input_cols = "x", n_boot = 20),
    class = "yourhonor_judge_error"
  )
  matrix_judge <- function(d) {
    out <- tibble::tibble(.item_id = d$.item_id)
    out$label <- matrix(rep("a", 2 * nrow(d)), ncol = 2)
    out
  }
  expect_error(
    calibrate_judge(gold, judge = matrix_judge, input_cols = "x", n_boot = 20),
    class = "yourhonor_judge_error"
  )
})

test_that("a judge that returns NaN abstains rather than labelling NaN", {
  gold <- tibble::tibble(
    id = 1:40, input = paste("q", 1:40), human_label = rep(c(1, 2), 20)
  )
  judge <- function(d) {
    item <- as.integer(sub("q ", "", d$input, fixed = TRUE))
    ifelse(item <= 4, NaN, rep(c(1, 2), 20)[item])
  }
  cal <- suppressWarnings(calibrate_judge(
    gold, judge, input_cols = "input", id_col = "id", n_boot = 20, seed = 1
  ))
  expect_equal(cal$n_judge_abstentions, 4)
  expect_false("NaN" %in% cal$label_levels)
  structured <- function(d) {
    tibble::tibble(.item_id = d$.item_id, label = judge(d))
  }
  cal2 <- suppressWarnings(calibrate_judge(
    gold, structured, input_cols = "input", id_col = "id", n_boot = 20,
    seed = 1
  ))
  expect_equal(cal2$n_judge_abstentions, 4)
})

test_that("a precomputed NaN rating is missing, not the label \"NaN\"", {
  human <- rep(c(1, 2), 20)
  judge <- human
  judge[1:4] <- NaN
  human[5:6] <- NaN
  gold <- tibble::tibble(id = 1:40, human_label = human, judge_label = judge)
  cal <- suppressWarnings(
    calibrate_judge(gold, id_col = "id", n_boot = 20, seed = 1)
  )
  expect_identical(cal$label_levels, c("1", "2"))
  expect_equal(cal$n_judge_abstentions, 4)
  expect_identical(sum(is.na(cal$items$human_label)), 2L)
  expect_identical(sum(is.na(cal$items$judge_label)), 4L)
  # Declared levels must not reject NaN as a label outside them.
  declared <- suppressWarnings(calibrate_judge(
    gold, id_col = "id", label_levels = c("1", "2"), n_boot = 20, seed = 1
  ))
  expect_identical(declared$n_complete, cal$n_complete)

  # Two human raters: a NaN is an unobserved rating in the consensus.
  multi <- tibble::tibble(
    id = 1:40, h1 = human, h2 = rep(c(1, 2), 20), judge_label = judge
  )
  cal_multi <- suppressWarnings(calibrate_judge(
    multi, human_cols = c("h1", "h2"), id_col = "id", n_boot = 20, seed = 1
  ))
  expect_identical(cal_multi$label_levels, c("1", "2"))
})

test_that("a NaN rating is missing in agreement() and confusion()", {
  human <- rep(c(1, 2), 20)
  judge <- human
  judge[1:4] <- NaN
  human[5:6] <- NaN
  ratings <- data.frame(human = human, judge = judge)
  ag <- agreement(ratings, rater_cols = c("human", "judge"), n_boot = 20)
  expect_identical(unique(ag$n_items), 34L)
  expect_identical(unique(ag$n_missing_rows), 6L)
  expect_equal(unique(ag$estimate), 1)
  cm <- confusion(ratings, human_col = "human", judge_col = "judge")
  expect_false("NaN" %in% c(cm$human_label, cm$judge_label))
  expect_identical(nrow(cm), 4L)
})

test_that("a NaN declared level is refused like NA, not kept as \"NaN\"", {
  gold <- tibble::tibble(
    human_label = rep(c(1, 2), 10), judge_label = rep(c(1, 2), 10)
  )
  expect_error(
    calibrate_judge(gold, label_levels = c(1, 2, NaN), n_boot = 20),
    "label_levels", class = "yourhonor_input_error"
  )
  expect_error(
    probe_bias(
      tibble::tibble(input = paste("answer", 1:6)),
      function(d) rep("1", nrow(d)), probes = "verbosity",
      ordered_levels = c(1, NaN)
    ),
    "ordered_levels", class = "yourhonor_input_error"
  )
  expect_error(
    probe_position(
      tibble::tibble(a = paste("A", 1:6), b = paste("B", 1:6)),
      function(d) tibble::tibble(.item_id = d$.item_id, label = "1"),
      "a", "b", choice_levels = c(1, NaN)
    ),
    "choice_levels", class = "yourhonor_input_error"
  )
})
