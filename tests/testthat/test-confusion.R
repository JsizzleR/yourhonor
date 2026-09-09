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
