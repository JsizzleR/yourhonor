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

test_that("class metrics are explicit and carry exact intervals", {
  ratings <- data.frame(
    human = c("C", "C", "P", "I"),
    judge = c("C", "C", "P", "I")
  )
  metrics <- confusion_metrics(
    ratings, label_levels = c("I", "P", "C")
  )
  expect_named(metrics, c(
    "label", "support", "predicted", "precision", "precision_conf_low",
    "precision_conf_high", "recall", "recall_conf_low", "recall_conf_high"
  ))
  expect_true(all(metrics$recall == 1))
  expect_true(all(metrics$recall_conf_low <= metrics$recall))
})

test_that("declared label order survives tables", {
  ratings <- data.frame(human = c("C", "P", "I"), judge = c("C", "P", "I"))
  conf <- confusion(ratings, label_levels = c("I", "P", "C"))
  expect_identical(unique(conf$human_label), c("I", "P", "C"))
  expect_identical(confusion_metrics(conf)$label, c("I", "P", "C"))
})
