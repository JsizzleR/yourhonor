# Numerical tolerance used when deciding whether an estimate or interval sits
# exactly on the [0, 1] boundary. A bootstrap that never leaves a boundary is a
# finite-sample artefact, not evidence of population-perfect agreement.
boundary_tolerance <- sqrt(.Machine$double.eps)

# Minimum number of finite bootstrap draws before an interval is trusted at all.
min_reliable_boot_draws <- 30L

# Fraction of requested resamples that must be finite before an interval is
# considered reliable; lower yields mean the statistic was often undefined.
min_effective_boot_fraction <- 0.8

# `class` lets a caller such as acceptance_policy() stamp every error it
# raises with one condition class, whichever shared validator fires.
check_count <- function(x,
                        arg = rlang::caller_arg(x),
                        minimum = 1L,
                        call = rlang::caller_env(),
                        class = NULL) {
  # Type is tested first: `is.na()` and the comparisons below are not defined
  # for complex or list input, and `as.integer()` warns above the integer
  # range instead of failing.
  ok <- is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x) &&
    x >= minimum
  if (ok && x > .Machine$integer.max) {
    cli::cli_abort(
      "{.arg {arg}} must be at most {(.Machine$integer.max)}.",
      call = call, class = class
    )
  }
  if (!ok || x != as.integer(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be a whole number greater than or equal to {minimum}.",
      call = call, class = class
    )
  }
  as.integer(x)
}

check_probability <- function(x,
                              arg = rlang::caller_arg(x),
                              open = FALSE,
                              call = rlang::caller_env(),
                              class = NULL) {
  ok <- is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x)
  if (ok) {
    ok <- if (open) x > 0 && x < 1 else x >= 0 && x <= 1
  }
  if (!ok) {
    bounds <- if (open) "strictly between 0 and 1" else "between 0 and 1"
    cli::cli_abort(
      "{.arg {arg}} must be {bounds}.", call = call, class = class
    )
  }
  as.numeric(x)
}

check_audit_id <- function(x,
                           arg = rlang::caller_arg(x),
                           allow_null = TRUE,
                           call = rlang::caller_env()) {
  if (is.null(x) && allow_null) return(NULL)
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be one non-empty character string.",
      call = call
    )
  }
  x
}

# Provenance is serialized verbatim into audit artifacts, so it must already
# be a named list; `modifyList()` downstream would otherwise launder bad input
# into something that merely looks well-formed.
check_provenance <- function(x,
                             arg = rlang::caller_arg(x),
                             call = rlang::caller_env()) {
  names_ok <- length(x) == 0L || (
    !is.null(names(x)) && !anyNA(names(x)) &&
      all(nzchar(trimws(names(x)))) && !anyDuplicated(names(x))
  )
  if (is.data.frame(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be a plain list, not a data frame.", call = call
    )
  }
  if (!is.list(x) || !names_ok) {
    cli::cli_abort(
      paste0(
        "{.arg {arg}} must be a list with unique, non-empty names ",
        "(an empty list is allowed)."
      ),
      call = call
    )
  }
  invisible(x)
}

check_column_names <- function(x,
                               arg = rlang::caller_arg(x),
                               minimum = 1L,
                               call = rlang::caller_env()) {
  if (!is.character(x) || length(x) < minimum || anyNA(x) ||
      any(!nzchar(x)) || anyDuplicated(x)) {
    cli::cli_abort(
      paste0(
        "{.arg {arg}} must contain at least {minimum} unique, non-empty ",
        "column name{?s}."
      ),
      call = call
    )
  }
  x
}

check_column_name <- function(x,
                              arg = rlang::caller_arg(x),
                              call = rlang::caller_env()) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be one non-empty column name.",
      call = call
    )
  }
  x
}

# Ensure every name in `cols` is a column of `data`; report the user's argument
# name (`arg`) and the user's data argument (`data_arg`) rather than internals.
check_columns_present <- function(data,
                                  cols,
                                  arg,
                                  data_arg = "x",
                                  call = rlang::caller_env()) {
  absent <- setdiff(cols, names(data))
  if (length(absent) > 0L) {
    # cli pluralises on the nearest preceding value, which would be `arg`;
    # `qty()` pins the count to the absent columns.
    cli::cli_abort(
      paste0(
        "{.arg {arg}} names {cli::qty(length(absent))}column{?s} not in ",
        "{.arg {data_arg}}: {.field {absent}}."
      ),
      call = call
    )
  }
  invisible(cols)
}

# `arg` names the user-facing source of `ratings` (a data argument or column)
# and is reported when those ratings hold blank labels; `levels_arg` names the
# caller's own label-universe argument, which differs between entry points
# (`label_levels`, `ordered_levels`, `choice_levels`).
resolve_label_levels <- function(ratings,
                                 label_levels = NULL,
                                 allow_single = FALSE,
                                 arg = "ratings",
                                 levels_arg = "label_levels",
                                 call = rlang::caller_env()) {
  observed <- unique(stats::na.omit(unlist(lapply(ratings, as.character), use.names = FALSE)))
  check_labels_nonblank(observed, arg, call = call)
  if (!is.null(label_levels)) {
    label_levels <- as.character(label_levels)
    minimum <- if (allow_single) 1L else 2L
    if (length(label_levels) < minimum || anyNA(label_levels) ||
        anyDuplicated(label_levels)) {
      cli::cli_abort(
        paste0(
          "{.arg {levels_arg}} must contain at least {minimum} unique, ",
          "non-missing label{?s}."
        ),
        call = call
      )
    }
    check_labels_nonblank(label_levels, levels_arg, call = call)
    unknown <- setdiff(observed, label_levels)
    if (length(unknown) > 0L) {
      # `qty()` gives the pluraliser its count; without it cli sees two
      # later substitutions and aborts with its own error.
      cli::cli_abort(
        paste0(
          "{cli::qty(length(unknown))}Observed label{?s} not present in ",
          "{.arg {levels_arg}}: {.val {unknown}}."
        ),
        call = call
      )
    }
    return(label_levels)
  }

  factor_levels <- unique(unlist(lapply(ratings, function(x) {
    if (is.factor(x)) levels(x) else character(0)
  }), use.names = FALSE))
  factor_levels <- factor_levels[!is_blank_label(factor_levels)]
  if (length(factor_levels) >= 2L) {
    # Declared factor order first, then any label observed only in a
    # non-factor column, so no observed rating is silently dropped.
    return(unique(c(factor_levels, observed)))
  }

  observed
}

# `arg` names the producer of `labels` (a judge or a column); `levels_arg` names
# the caller's own label-universe argument, which differs between entry points
# (`label_levels`, `ordered_levels`, `choice_levels`).
validate_labels <- function(labels,
                            label_levels,
                            arg = "judge",
                            levels_arg = "label_levels",
                            call = rlang::caller_env()) {
  labels <- as.character(labels)
  unknown <- setdiff(unique(stats::na.omit(labels)), label_levels)
  if (length(unknown) > 0L) {
    cli::cli_abort(
      paste0(
        "{.arg {arg}} returned label{?s} outside {.arg {levels_arg}}: ",
        "{.val {unknown}}."
      ),
      call = call
    )
  }
  labels
}

observed_categories <- function(ratings) {
  values <- if (is.list(ratings)) {
    unlist(lapply(ratings, as.character), use.names = FALSE)
  } else {
    as.vector(ratings)
  }
  unique(stats::na.omit(values))
}

# Blank labels are missing values, not categories. irrCAC silently maps "" to
# NA, so accepting them here would make agreement disagree with confusion().
is_blank_label <- function(labels) {
  !is.na(labels) & !nzchar(trimws(labels, whitespace = "[\\h\\v]"))
}

check_labels_nonblank <- function(labels, arg, call = rlang::caller_env()) {
  if (any(is_blank_label(labels))) {
    cli::cli_abort(
      c(
        "{.arg {arg}} contains empty or whitespace-only labels.",
        i = "Represent a missing or failed judgement as {.code NA}, not {.val {''}}."
      ),
      call = call
    )
  }
  invisible(labels)
}

# irrCAC upper-cases and trims character ratings and category labels before
# comparing them, so labels that differ only by case or whitespace would be
# merged and double counted. Integer codes bypass that path entirely and keep
# the package's exact-match label universe, in declared order, for weighting.
encode_ratings <- function(ratings, label_levels) {
  ratings <- as.data.frame(ratings, stringsAsFactors = FALSE)
  coded <- lapply(ratings, function(col) {
    values <- as.character(col)
    codes <- match(values, label_levels)
    if (any(is.na(codes) & !is.na(values))) {
      cli::cli_abort("Internal error: a rating is outside the resolved label universe.")
    }
    codes
  })
  as.data.frame(coded, stringsAsFactors = FALSE, col.names = names(ratings))
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
    ci_reliable = length(stat) >= min_reliable_boot_draws
  )
}

# A perfect finite sample gives a zero-width bootstrap pinned at 1. That is a
# property of the sample, not proof of population-perfect agreement, so it is
# flagged rather than reported as a reliable interval.
is_boundary_degenerate <- function(status, estimate, ci) {
  identical(status, "ok") && is.finite(estimate) &&
    estimate >= 1 - boundary_tolerance &&
    is.finite(ci$se) && abs(ci$se) <= boundary_tolerance &&
    is.finite(ci$conf_low) && is.finite(ci$conf_high) &&
    abs(ci$conf_high - ci$conf_low) <= boundary_tolerance
}

# An interval is reliable when enough draws were finite, most of the requested
# resamples produced a defined statistic, and the interval is not degenerate.
ci_is_reliable <- function(ci, n_boot, boundary_degenerate = FALSE) {
  isTRUE(ci$ci_reliable) &&
    ci$n_effective >= ceiling(min_effective_boot_fraction * n_boot) &&
    !isTRUE(boundary_degenerate)
}
