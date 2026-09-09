test_that("confusion counts and row proportions are correct", {
  ratings <- data.frame(
    human = c("C", "C", "P", "I"),
    judge = c("C", "P", "P", "I")
  )
  conf <- confusion(
    ratings, normalize = "row", label_levels = c("I", "P", "C")
  )
  expect_named(conf, c("human_label", "judge_label", "n", "prop"))
  cc <- conf$n[conf$human_label == "C" & conf$judge_label == "C"]
  cp <- conf$n[conf$human_label == "C" & conf$judge_label == "P"]
  expect_equal(cc, 1)
  expect_equal(cp, 1)
  expect_equal(sum(conf$prop[conf$human_label == "C"]), 1)
})

test_that("whole-table proportions sum to one", {
  ratings <- data.frame(
    human = c("C", "C", "P", "I", "C", "P", "I", "C"),
    judge = c("C", "C", "C", "I", "C", "P", "P", "C")
  )
  conf <- confusion(ratings, normalize = "none", label_levels = c("I", "P", "C"))
  expect_equal(sum(conf$prop), 1)
  expect_equal(conf$prop[conf$human_label == "C" & conf$judge_label == "C"], 0.5)
})

test_that("column normalization sums to one per judge label", {
  ratings <- data.frame(
    human = c("C", "C", "P", "I", "C", "P", "I", "C"),
    judge = c("C", "C", "C", "I", "C", "P", "P", "C")
  )
  conf <- confusion(ratings, normalize = "col", label_levels = c("I", "P", "C"))
  sums <- tapply(conf$prop, conf$judge_label, sum)
  expect_equal(as.vector(sums[c("I", "P", "C")]), c(1, 1, 1))
  p_as_c <- conf$human_label == "P" & conf$judge_label == "C"
  expect_equal(conf$prop[p_as_c], 0.2)
})

test_that("class metrics are exact on imperfect data", {
  ratings <- data.frame(
    human = c("C", "C", "P", "I", "C", "P", "I", "C"),
    judge = c("C", "C", "C", "I", "C", "P", "P", "C")
  )
  metrics <- confusion_metrics(ratings, label_levels = c("I", "P", "C"))
  expect_named(metrics, c(
    "label", "support", "predicted", "precision", "precision_conf_low",
    "precision_conf_high", "recall", "recall_conf_low", "recall_conf_high"
  ))
  expect_identical(metrics$label, c("I", "P", "C"))
  expect_equal(metrics$support, c(2L, 2L, 4L))
  expect_equal(metrics$predicted, c(1L, 2L, 5L))
  expect_equal(metrics$recall, c(0.5, 0.5, 1))
  expect_equal(metrics$precision, c(1, 0.5, 0.8))
  cp <- binom.test(4, 5)$conf.int
  expect_equal(metrics$precision_conf_low[[3]], cp[[1]])
  expect_equal(metrics$precision_conf_high[[3]], cp[[2]])
  rc <- binom.test(1, 2)$conf.int
  expect_equal(metrics$recall_conf_low[[1]], rc[[1]])
  expect_equal(metrics$recall_conf_high[[1]], rc[[2]])
})

test_that("a declared but unobserved label has undefined metrics", {
  ratings <- data.frame(human = c("C", "P", "C"), judge = c("C", "P", "P"))
  metrics <- confusion_metrics(ratings, label_levels = c("I", "P", "C"))
  i <- metrics[metrics$label == "I", ]
  expect_equal(i$support, 0L)
  expect_equal(i$predicted, 0L)
  expect_true(is.na(i$precision))
  expect_true(is.na(i$recall))
  expect_true(is.na(i$precision_conf_low))
  expect_true(is.na(i$recall_conf_high))
})

test_that("confusion errors are explicit", {
  expect_error(
    confusion(data.frame(human = c(NA, NA), judge = c("C", "P"))),
    "No complete"
  )
  expect_error(
    confusion(data.frame(a = "C", b = "C", c = "C")),
    "human_col"
  )
  expect_error(
    confusion(data.frame(human = "C", judge = "C"), human_col = "human"),
    "both"
  )
  expect_error(
    confusion(
      data.frame(human = "C", judge = "C"), human_col = "human",
      judge_col = "human"
    ),
    "different"
  )
  expect_error(
    confusion(
      data.frame(human = "C", judge = "C"), human_col = "human",
      judge_col = "nope"
    ),
    "nope"
  )
})

test_that("rating columns are explicit or inferred with a message", {
  ratings <- data.frame(
    h = c("C", "C", "P", "I"), j = c("C", "P", "P", "I"), extra = 1:4
  )
  explicit <- confusion(
    ratings, human_col = "h", judge_col = "j", label_levels = c("I", "P", "C")
  )
  c_as_p <- explicit$human_label == "C" & explicit$judge_label == "P"
  expect_equal(explicit$n[c_as_p], 1)

  expect_message(
    positional <- confusion(
      ratings[c("h", "j")], label_levels = c("I", "P", "C")
    ),
    class = "yourhonor_inferred_columns"
  )
  expect_equal(positional, explicit)

  named <- ratings
  names(named) <- c("human", "judge", "extra")
  expect_message(confusion(named), "Ignoring column")
  expect_silent(confusion(named[c("human", "judge")]))
})

test_that("confusion_metrics reads a calibration and passes columns through", {
  gold <- tibble::tibble(
    input = paste("answer", 1:8),
    human_label = c("C", "C", "P", "I", "C", "P", "I", "C"),
    judge_label = c("C", "C", "C", "I", "C", "P", "P", "C")
  )
  cal <- calibrate_judge(
    gold, label_levels = c("I", "P", "C"), n_boot = 20, seed = 1
  )
  expect_identical(confusion_metrics(cal), cal$confusion_metrics)
  from_frame <- confusion_metrics(
    gold, label_levels = c("I", "P", "C"), human_col = "human_label",
    judge_col = "judge_label"
  )
  expect_equal(from_frame, cal$confusion_metrics)
})

test_that("declared label order survives tables", {
  ratings <- data.frame(human = c("C", "P", "I"), judge = c("C", "P", "I"))
  conf <- confusion(ratings, label_levels = c("I", "P", "C"))
  expect_identical(unique(conf$human_label), c("I", "P", "C"))
  expect_identical(confusion_metrics(conf)$label, c("I", "P", "C"))
  expect_identical(attr(conf, "label_levels", exact = TRUE), c("I", "P", "C"))
})

test_that("plot_confusion keeps the declared label order on both axes", {
  skip_if_not_installed("ggplot2")
  ratings <- data.frame(human = c("C", "P", "I"), judge = c("C", "P", "I"))
  p <- plot_confusion(ratings, label_levels = c("I", "P", "C"))
  expect_s3_class(p, "ggplot")
  expect_identical(levels(p$data$judge_label), c("I", "P", "C"))
  # Reversed so the agreement diagonal reads from top-left to bottom-right.
  expect_identical(levels(p$data$human_label), c("C", "P", "I"))

  gold <- tibble::tibble(
    input = paste("answer", 1:6),
    human_label = c("C", "P", "I", "C", "P", "I"),
    judge_label = c("C", "P", "I", "C", "I", "I")
  )
  cal <- calibrate_judge(
    gold, label_levels = c("I", "P", "C"), n_boot = 20, seed = 1
  )
  p_cal <- plot_confusion(cal)
  expect_identical(levels(p_cal$data$judge_label), c("I", "P", "C"))
})

test_that("explicit columns cannot be combined with a calibration", {
  gold <- tibble::tibble(
    human_label = c("C", "C", "P", "I", "C", "P", "I", "C"),
    judge_label = c("C", "C", "C", "I", "C", "P", "P", "C")
  )
  cal <- calibrate_judge(
    gold, label_levels = c("I", "P", "C"), n_boot = 20, seed = 1
  )
  expect_error(
    confusion(cal, human_col = "zzz", judge_col = "yyy"),
    "cannot be combined with a calibration"
  )
  expect_error(
    confusion(cal, human_col = "human_label"),
    "cannot be combined with a calibration"
  )
  expect_error(
    confusion_metrics(cal, judge_col = "judge_label"),
    "cannot be combined with a calibration"
  )
  expect_error(
    confusion_metrics(confusion(cal), human_col = "a", judge_col = "b"),
    "cannot be combined with a confusion tibble"
  )
  conf <- confusion(data.frame(human = c("C", "P"), judge = c("C", "P")))
  expect_error(
    confusion_metrics(conf, human_col = "human"),
    "cannot be combined with a confusion tibble"
  )
})

test_that("confusion_metrics errors name confusion_metrics()", {
  err <- rlang::catch_cnd(
    confusion_metrics(data.frame(a = 1, b = 2, c = 3)), "error"
  )
  expect_identical(rlang::call_name(err$call), "confusion_metrics")
  err <- rlang::catch_cnd(
    suppressMessages(
      confusion_metrics(data.frame(a = "x", b = "y"), label_levels = "x")
    ),
    "error"
  )
  expect_identical(rlang::call_name(err$call), "confusion_metrics")
})

test_that("positional inference asks for both selectors", {
  expect_message(
    confusion(data.frame(a = c("x", "y"), b = c("x", "y"))),
    "Set `human_col` and `judge_col` to choose explicitly",
    class = "yourhonor_inferred_columns"
  )
})

test_that("confusion_metrics applies label_levels to cached metrics", {
  ratings <- data.frame(
    human = c("C", "C", "P", "I", "C", "P", "I", "C"),
    judge = c("C", "C", "C", "I", "C", "P", "P", "C")
  )
  conf <- confusion(ratings, label_levels = c("I", "P", "C"))
  reordered <- confusion_metrics(conf, label_levels = c("C", "P", "I", "X"))
  expect_identical(reordered$label, c("C", "P", "I", "X"))
  base <- confusion_metrics(conf)
  expect_identical(
    reordered[1:3, ], base[match(c("C", "P", "I"), base$label), ]
  )
  expect_identical(reordered$support[[4]], 0L)
  expect_identical(reordered$predicted[[4]], 0L)
  expect_true(is.na(reordered$recall[[4]]))
  expect_error(
    confusion_metrics(conf, label_levels = c("C", "P")),
    'Observed label not present in `label_levels`: "I"',
    fixed = TRUE
  )
  cal <- calibrate_judge(
    tibble::tibble(human_label = ratings$human, judge_label = ratings$judge),
    label_levels = c("I", "P", "C"), n_boot = 20, seed = 1
  )
  expect_identical(
    confusion_metrics(cal, label_levels = c("P", "I", "C"))$label,
    c("P", "I", "C")
  )
})

test_that("duplicated column names are rejected before selection", {
  ratings <- data.frame(c("A", "B"), c("A", "B"))
  names(ratings) <- c("human", "human")
  expect_error(
    confusion(ratings, human_col = "human", judge_col = "judge"),
    "duplicated column name: human"
  )
  expect_error(confusion(ratings), "duplicated column name: human")
  twice <- data.frame(1, 2, 3, 4)
  names(twice) <- c("human", "human", "judge", "judge")
  expect_error(
    confusion(twice, human_col = "human", judge_col = "judge"),
    "duplicated column names: human and judge"
  )
})
