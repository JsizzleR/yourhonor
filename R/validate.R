check_count <- function(x, arg, minimum = 1L) {
  if (length(x) != 1L || is.na(x) || !is.numeric(x) ||
      !is.finite(x) || x != as.integer(x) || x < minimum) {
    cli::cli_abort(
      "{.arg {arg}} must be a whole number greater than or equal to {minimum}."
    )
  }
  as.integer(x)
}

check_probability <- function(x, arg, open = FALSE) {
  ok <- length(x) == 1L && !is.na(x) && is.numeric(x) && is.finite(x)
  if (ok) {
    ok <- if (open) x > 0 && x < 1 else x >= 0 && x <= 1
  }
  if (!ok) {
    bounds <- if (open) "strictly between 0 and 1" else "between 0 and 1"
    cli::cli_abort("{.arg {arg}} must be {bounds}.")
  }
  as.numeric(x)
}

check_audit_id <- function(x, arg = "audit_id", allow_null = TRUE) {
  if (is.null(x) && allow_null) return(NULL)
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    cli::cli_abort("{.arg {arg}} must be one non-empty character string.")
  }
  x
}

check_column_names <- function(x, arg, minimum = 1L) {
  if (!is.character(x) || length(x) < minimum || anyNA(x) ||
      any(!nzchar(x)) || anyDuplicated(x)) {
    cli::cli_abort(
      "{.arg {arg}} must contain at least {minimum} unique, non-empty column name{?s}."
    )
  }
  x
}

check_column_name <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    cli::cli_abort("{.arg {arg}} must be one non-empty column name.")
  }
  x
}

resolve_label_levels <- function(ratings, label_levels = NULL, allow_single = FALSE) {
  if (!is.null(label_levels)) {
    label_levels <- as.character(label_levels)
    if ((!allow_single && length(label_levels) < 2L) || length(label_levels) < 1L ||
        anyNA(label_levels) || anyDuplicated(label_levels)) {
      cli::cli_abort(
        "{.arg label_levels} must contain at least two unique, non-missing labels."
      )
    }
    observed <- unique(stats::na.omit(unlist(lapply(ratings, as.character), use.names = FALSE)))
    unknown <- setdiff(observed, label_levels)
    if (length(unknown) > 0L) {
      cli::cli_abort(
        "Observed label{?s} not present in {.arg label_levels}: {.val {unknown}}."
      )
    }
    return(label_levels)
  }

  factor_levels <- unique(unlist(lapply(ratings, function(x) {
    if (is.factor(x)) levels(x) else character(0)
  }), use.names = FALSE))
  if (length(factor_levels) >= 2L) {
    return(factor_levels)
  }

  unique(stats::na.omit(unlist(lapply(ratings, as.character), use.names = FALSE)))
}

validate_labels <- function(labels, label_levels, arg = "judge") {
  labels <- as.character(labels)
  unknown <- setdiff(unique(stats::na.omit(labels)), label_levels)
  if (length(unknown) > 0L) {
    cli::cli_abort(
      "{.arg {arg}} returned label{?s} outside {.arg label_levels}: {.val {unknown}}."
    )
  }
  labels
}

observed_categories <- function(ratings) {
  unique(stats::na.omit(unlist(ratings, use.names = FALSE)))
}

exact_binom_interval <- function(successes, trials, conf_level = 0.95) {
  if (trials == 0L) {
    return(c(conf_low = NA_real_, conf_high = NA_real_))
  }
  unname(stats::binom.test(successes, trials, conf.level = conf_level)$conf.int)
}

bootstrap_quantiles <- function(stat, conf_level) {
  stat <- stat[is.finite(stat)]
  if (length(stat) == 0L) {
    return(list(
      conf_low = NA_real_, conf_high = NA_real_, se = NA_real_,
      n_effective = 0L, ci_reliable = FALSE
    ))
  }
  alpha <- (1 - conf_level) / 2
  qs <- stats::quantile(stat, c(alpha, 1 - alpha), names = FALSE)
  list(
    conf_low = qs[[1]],
    conf_high = qs[[2]],
    se = stats::sd(stat),
    n_effective = length(stat),
    ci_reliable = length(stat) >= 30L
  )
}
