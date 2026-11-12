# expect_all() (helper-expect.R) stands in for expect_true(all(x)) across the
# suite, so it must pass and fail where that did and report what failed.

test_that("expect_all() passes and fails where expect_true(all(x)) does", {
  expect_success(expect_all(c(TRUE, TRUE)))
  expect_success(expect_all(logical(0)))
  expect_failure(expect_all(c(TRUE, FALSE)))
  expect_failure(expect_all(c(TRUE, NA)))
  expect_failure(expect_all(c(1, 2)), "not a logical vector")
})

test_that("expect_all() names the failing positions and compared values", {
  status <- c("ok", "bad", "ok", NA)
  expect_failure(
    expect_all(status == "ok"),
    "2 of 4 are not, at 2, 4\\.\nstatus: bad, NA\n\"ok\": ok"
  )
  # Names are shown beside positions, so blank or repeated ones stay clear.
  x <- c(a = TRUE, a = FALSE, FALSE)
  names(x)[3] <- ""
  expect_failure(expect_all(x), "at 2 \\(\"a\"\\), 3 \\(\"\"\\)\\.")
})

test_that("expect_all() evaluates a comparison's operands once", {
  calls <- 0
  draw <- function() {
    calls <<- calls + 1
    c(1, 5)
  }
  expect_failure(expect_all(draw() < 3), "draw\\(\\): 5")
  expect_identical(calls, 1)
  calls <- 0
  expect_success(expect_all(draw() > 0))
  expect_identical(calls, 1)
})

test_that("expect_none() passes and fails where expect_false(any(x)) does", {
  expect_success(expect_none(c(FALSE, FALSE)))
  expect_success(expect_none(logical(0)))
  expect_failure(expect_none(c(FALSE, TRUE)))
  expect_failure(expect_none(c(FALSE, NA)))
  expect_failure(expect_none(c(0, 1)), "not a logical vector")
})

test_that("expect_none() names the offending positions and compared values", {
  seen <- c("text", "human_label", "id", NA)
  expect_failure(
    expect_none(seen %in% c("human_label", "id")),
    paste0(
      "every element of `seen %in% c\\(\"human_label\", \"id\"\\)` to be ",
      "FALSE; 2 of 4 are not, at 2, 3\\.\nseen: human_label, id\n",
      "c\\(\"human_label\", \"id\"\\): human_label, id"
    )
  )
  flags <- c(a = FALSE, b = TRUE, c = NA)
  expect_failure(expect_none(flags), "at 2 \\(\"b\"\\), 3 \\(\"c\"\\)\\.")
})

test_that("the table of %in% is shown whole, not at the failing positions", {
  # Equal in length to the result, the table was once indexed like an
  # elementwise operand, hiding the value that matched.
  expect_failure(
    expect_none(c("b", "z") %in% c("a", "b")),
    "at 1\\.\nc\\(\"b\", \"z\"\\): b\nc\\(\"a\", \"b\"\\): a, b$"
  )
})
