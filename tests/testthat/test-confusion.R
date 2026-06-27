test_that("confusion counts and proportions are correct", {
  ratings <- data.frame(
    human = c("C", "C", "P", "I"),
    judge = c("C", "P", "P", "I")
  )
  conf <- confusion(ratings, normalize = "row")
  expect_named(conf, c("human_label", "judge_label", "n", "prop"))

  cc <- conf$n[conf$human_label == "C" & conf$judge_label == "C"]
  cp <- conf$n[conf$human_label == "C" & conf$judge_label == "P"]
  expect_equal(cc, 1)
  expect_equal(cp, 1)

  row_c <- conf$prop[conf$human_label == "C"]
  expect_equal(sum(row_c), 1)
})

test_that("normalize = 'none' proportions sum to 1 (regression: ifelse recycling)", {
  ratings <- data.frame(
    human = c("C", "C", "P", "I", "C", "P", "I", "C"),
    judge = c("C", "C", "C", "I", "C", "P", "P", "C")
  )
  conf <- confusion(ratings, normalize = "none")
  expect_equal(sum(conf$prop), 1)
  # the largest cell (C,C = 4 of 8) should be 0.5, not every cell at 0.5
  cc <- conf$prop[conf$human_label == "C" & conf$judge_label == "C"]
  expect_equal(cc, 0.5)
  expect_false(all(conf$prop == conf$prop[1]))
})

test_that("per-class precision and recall are attached", {
  ratings <- data.frame(
    human = c("C", "C", "P", "I"),
    judge = c("C", "C", "P", "I")
  )
  conf <- confusion(ratings)
  m <- attr(conf, "metrics")
  expect_s3_class(m, "tbl_df")
  expect_true(all(m$recall == 1))
  expect_true(all(m$precision == 1))
})
