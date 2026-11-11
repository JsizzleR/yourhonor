# Every element of `object` is TRUE. `expect_true(all(x))` fails with a bare
# "is not TRUE"; this names the expression, the positions that are FALSE or
# NA (with their names, when the vector has them) and, for a comparison such
# as `x == "ok"` or `a <= b`, the values on each side at those positions.
# An empty logical vector passes, as `all(logical(0))` does; anything other
# than a logical vector fails, since all() would coerce it.
expect_all <- function(object, info = NULL, label = NULL) {
  quo <- rlang::enquo(object)
  expr <- rlang::quo_get_expr(quo)
  env <- rlang::quo_get_env(quo)
  lab <- if (is.null(label)) {
    paste0("`", paste(rlang::expr_deparse(expr), collapse = " "), "`")
  } else {
    label
  }
  # A comparison's operands are evaluated once and compared here, so the
  # values a failure reports are the ones that were compared.
  ops <- c("==", "!=", "<", "<=", ">", ">=", "%in%")
  operands <- NULL
  if (rlang::is_call(expr, ops, n = 2L)) {
    operands <- list(eval(expr[[2]], env), eval(expr[[3]], env))
    op <- get(rlang::call_name(expr), envir = env, mode = "function")
    x <- op(operands[[1]], operands[[2]])
  } else {
    x <- rlang::eval_tidy(quo)
  }
  if (!is.logical(x)) {
    testthat::expect(
      FALSE, sprintf("%s is %s, not a logical vector.", lab, class(x)[[1]]),
      info = info
    )
    return(invisible(x))
  }
  bad <- which(is.na(x) | !x)
  if (length(bad) == 0L) {
    testthat::expect(TRUE, "", info = info)
    return(invisible(x))
  }
  shown <- utils::head(bad, 10L)
  where <- if (is.null(names(x))) {
    shown
  } else {
    sprintf("%d (%s)", shown, encodeString(names(x)[shown], quote = "\""))
  }
  msg <- sprintf(
    "Expected every element of %s to be TRUE; %d of %d %s not, at %s%s.",
    lab, length(bad), length(x), if (length(bad) == 1L) "is" else "are",
    paste(where, collapse = ", "), if (length(bad) > 10L) ", ..." else ""
  )
  if (!is.null(operands)) {
    sides <- vapply(1:2, function(i) {
      operand_values(operands[[i]], expr[[i + 1L]], shown, length(x))
    }, character(1))
    msg <- paste0(msg, paste(sides, collapse = ""))
  }
  testthat::expect(FALSE, msg, info = info)
  invisible(x)
}

# One operand of a failed comparison: its values at the failing positions
# when it is as long as the result, otherwise its first ten (a recycled
# scalar, or the table of `%in%`). "" for anything that is not atomic.
operand_values <- function(value, expr, positions, n) {
  if (!is.atomic(value) || is.null(value)) return("")
  picked <- if (length(value) == n) {
    value[positions]
  } else {
    utils::head(value, 10L)
  }
  shown <- tryCatch(
    if (is.numeric(picked)) {
      format(picked, digits = 6, trim = TRUE)
    } else {
      as.character(picked)
    },
    error = function(err) "<unprintable>"
  )
  sprintf(
    "\n%s: %s", paste(rlang::expr_deparse(expr), collapse = " "),
    paste(shown, collapse = ", ")
  )
}
