#' Declare a judge acceptance policy
#'
#' A policy turns descriptive calibration evidence into a decision. Thresholds
#' are intentionally supplied by the caller because acceptable error depends on
#' the task and the cost of each class of mistake.
#'
#' Stability and bias thresholds are grouped behind a parent gate:
#' `max_pairwise_disagreement` switches the stability checks on and
#' `required_probes` switches the bias checks on. The dependent thresholds must
#' be supplied together with their gate and are rejected without it, so a policy
#' can never carry a threshold that the assessment does not read.
#'
#' @param min_agreement_lower Minimum acceptable lower confidence bound for the
#'   selected agreement coefficient.
#' @param min_n Minimum usable judge-human pairs.
#' @param min_class_n Minimum human-reference support for every class.
#' @param min_recall Minimum lower confidence bound for per-class recall. A
#'   scalar applies to all classes; a named vector applies only to named classes.
#' @param max_missing_rate Maximum fraction of calibration rows without a usable
#'   judge-consensus pair.
#' @param max_pairwise_disagreement Maximum stability disagreement. Set `NULL`
#'   explicitly when stability is outside this policy.
#' @param min_stability_n Minimum independent items with at least two stability
#'   judgments. Required when `max_pairwise_disagreement` is not `NULL` and
#'   rejected otherwise.
#' @param min_stability_runs Minimum repeated judge runs. Required when
#'   `max_pairwise_disagreement` is not `NULL` and rejected otherwise.
#' @param max_stability_missing_rate Maximum fraction of individual repeated
#'   judgments that may be missing. Required when stability is in the policy
#'   and rejected otherwise.
#' @param required_probes Bias probes that must be present and free of detected
#'   signal.
#' @param min_probe_n Minimum paired comparisons required for every required
#'   bias probe. Required when `required_probes` is non-empty and rejected
#'   otherwise.
#' @param max_probe_flip_rate Maximum upper confidence bound for a required
#'   probe's flip rate. Required when `required_probes` is non-empty and
#'   rejected otherwise.
#' @param max_probe_directional_rate Maximum practically tolerable net
#'   directional effect, assessed against a conservative confidence interval.
#'   Required when `required_probes` is non-empty and rejected otherwise.
#' @param max_probe_missing_rate Maximum fraction of individual probe judgments
#'   that may be missing. Required when `required_probes` is non-empty so
#'   selective abstention cannot make a probe look clean; rejected otherwise.
#' @param min_precision Optional minimum lower confidence bound for per-class
#'   precision, scalar or named.
#' @param min_human_reliability_lower Optional minimum lower confidence bound
#'   for human-rater Gwet AC1 (nominal) or AC2 (weighted). When supplied, a
#'   single-rater gold set is `NOT_ASSESSED`, never accepted silently.
#' @param conf_level Confidence level required across calibration, bias, and
#'   stability evidence.
#' @param coefficient Agreement coefficient used by the policy.
#'
#' @return A `yourhonor_policy` object.
#' @export
acceptance_policy <- function(min_agreement_lower,
                              min_n,
                              min_class_n,
                              min_recall,
                              max_missing_rate,
                              max_pairwise_disagreement,
                              min_stability_n = NULL,
                              min_stability_runs = NULL,
                              max_stability_missing_rate = NULL,
                              required_probes = character(0),
                              min_probe_n = NULL,
                              max_probe_flip_rate = NULL,
                              max_probe_directional_rate = NULL,
                              max_probe_missing_rate = NULL,
                              min_precision = NULL,
                              min_human_reliability_lower = NULL,
                              conf_level = 0.95,
                              coefficient = c(
                                "gwet_ac1", "gwet_ac2", "cohen_kappa",
                                "krippendorff_alpha"
                              )) {
  rlang::check_required(min_agreement_lower)
  rlang::check_required(min_n)
  rlang::check_required(min_class_n)
  rlang::check_required(min_recall)
  rlang::check_required(max_missing_rate)
  rlang::check_required(max_pairwise_disagreement)
  coefficient <- rlang::arg_match(coefficient)
  conf_level <- check_probability(conf_level, open = TRUE)
  min_agreement_lower <- check_numeric_threshold(min_agreement_lower)
  min_n <- check_count(min_n, minimum = 2L)
  min_class_n <- check_count(min_class_n)
  min_recall <- check_metric_threshold(min_recall)
  max_missing_rate <- check_probability(max_missing_rate)
  min_precision <- check_optional(min_precision, check_metric_threshold)
  min_human_reliability_lower <- check_optional(
    min_human_reliability_lower, check_numeric_threshold
  )

  max_pairwise_disagreement <- check_optional(
    max_pairwise_disagreement, check_probability
  )
  check_dependent_thresholds(
    list(
      min_stability_n = min_stability_n,
      min_stability_runs = min_stability_runs,
      max_stability_missing_rate = max_stability_missing_rate
    ),
    gate_on = !is.null(max_pairwise_disagreement),
    gate = "max_pairwise_disagreement",
    gate_state = "is not NULL"
  )
  min_stability_n <- check_optional(min_stability_n, check_count)
  min_stability_runs <- check_optional(
    min_stability_runs, check_count, minimum = 2L
  )
  max_stability_missing_rate <- check_optional(
    max_stability_missing_rate, check_probability
  )

  required_probes <- check_required_probes(required_probes)
  check_dependent_thresholds(
    list(
      min_probe_n = min_probe_n,
      max_probe_flip_rate = max_probe_flip_rate,
      max_probe_directional_rate = max_probe_directional_rate,
      max_probe_missing_rate = max_probe_missing_rate
    ),
    gate_on = length(required_probes) > 0L,
    gate = "required_probes",
    gate_state = "is non-empty"
  )
  min_probe_n <- check_optional(min_probe_n, check_count)
  max_probe_flip_rate <- check_optional(max_probe_flip_rate, check_probability)
  max_probe_directional_rate <- check_optional(
    max_probe_directional_rate, check_probability
  )
  max_probe_missing_rate <- check_optional(
    max_probe_missing_rate, check_probability
  )

  structure(
    list(
      coefficient = coefficient,
      min_agreement_lower = min_agreement_lower,
      min_n = min_n,
      min_class_n = min_class_n,
      min_recall = min_recall,
      min_precision = min_precision,
      min_human_reliability_lower = min_human_reliability_lower,
      max_missing_rate = max_missing_rate,
      max_pairwise_disagreement = max_pairwise_disagreement,
      min_stability_n = min_stability_n,
      min_stability_runs = min_stability_runs,
      max_stability_missing_rate = max_stability_missing_rate,
      required_probes = required_probes,
      min_probe_n = min_probe_n,
      max_probe_flip_rate = max_probe_flip_rate,
      max_probe_directional_rate = max_probe_directional_rate,
      max_probe_missing_rate = max_probe_missing_rate,
      conf_level = conf_level
    ),
    class = "yourhonor_policy"
  )
}

# Every probe a policy may require. Kept in one place so the policy and the
# probe implementations cannot drift apart silently.
bias_probe_names <- c("verbosity", "self_preference", "bandwagon", "position")

check_required_probes <- function(x,
                                  arg = rlang::caller_arg(x),
                                  call = rlang::caller_env()) {
  if (!is.character(x) || anyNA(x) || any(!nzchar(x))) {
    cli::cli_abort(
      "{.arg {arg}} must be a character vector of probe names.",
      class = "yourhonor_policy_error", call = call
    )
  }
  unknown <- setdiff(x, bias_probe_names)
  if (length(unknown) > 0L) {
    cli::cli_abort(
      c(
        "Unknown {.arg {arg}}: {.val {unknown}}.",
        i = "Known probes: {.val {bias_probe_names}}."
      ),
      class = "yourhonor_policy_error", call = call
    )
  }
  if (anyDuplicated(x)) {
    cli::cli_abort(
      paste0(
        "{.arg {arg}} must not repeat a probe: ",
        "{.val {unique(x[duplicated(x)])}}."
      ),
      class = "yourhonor_policy_error", call = call
    )
  }
  x
}

# Validate `x` with `check` unless it is NULL. The caller's argument name is
# forwarded so an error names the policy argument rather than `x`.
check_optional <- function(x,
                           check,
                           ...,
                           arg = rlang::caller_arg(x),
                           call = rlang::caller_env()) {
  if (is.null(x)) return(NULL)
  check(x, arg = arg, ..., call = call)
}

# Thresholds that only exist inside a parent gate must travel with it: when
# the gate is on every one of them is read, and when it is off none of them
# is. A stored-but-unread threshold would make the policy look stricter than
# the assessment actually is, so both directions are errors.
check_dependent_thresholds <- function(thresholds,
                                       gate_on,
                                       gate,
                                       gate_state,
                                       call = rlang::caller_env()) {
  supplied <- !vapply(thresholds, is.null, logical(1))
  args <- names(thresholds)
  if (gate_on && !all(supplied)) {
    cli::cli_abort(
      paste0(
        "{.arg {args[!supplied]}} {?is/are} required when ",
        "{.arg {gate}} {gate_state}."
      ),
      class = "yourhonor_policy_error", call = call
    )
  }
  if (!gate_on && any(supplied)) {
    cli::cli_abort(
      c(
        paste0(
          "{.arg {args[supplied]}} {?was/were} supplied but ",
          "{.arg {gate}} {gate_state} only when that gate is in the policy."
        ),
        i = "A threshold behind an inactive gate is never assessed.",
        i = "Either drop {?it/them} or supply {.arg {gate}}."
      ),
      class = "yourhonor_policy_error", call = call
    )
  }
  invisible(NULL)
}

check_numeric_threshold <- function(x,
                                    arg = rlang::caller_arg(x),
                                    call = rlang::caller_env()) {
  if (length(x) != 1L || is.na(x) || !is.numeric(x) || !is.finite(x) ||
      x < -1 || x > 1) {
    cli::cli_abort(
      "{.arg {arg}} must be one finite number between -1 and 1.",
      call = call
    )
  }
  as.numeric(x)
}

check_metric_threshold <- function(x,
                                   arg = rlang::caller_arg(x),
                                   call = rlang::caller_env()) {
  names_invalid <- !is.null(names(x)) &&
    (anyNA(names(x)) || any(!nzchar(names(x))) || anyDuplicated(names(x)))
  if (!is.numeric(x) || length(x) < 1L || anyNA(x) || any(!is.finite(x)) ||
      any(x < 0 | x > 1) || (length(x) > 1L && is.null(names(x))) ||
      names_invalid) {
    cli::cli_abort(
      paste0(
        "{.arg {arg}} must be a scalar or fully named numeric vector ",
        "between 0 and 1."
      ),
      call = call
    )
  }
  x
}

# Assessment ------------------------------------------------------------------

# Every check row carries the comparison it encodes (`op` and a numeric
# `threshold`) next to the observed `value`, so renderers and the JSON
# artifact can show a failure's magnitude without parsing prose.
assess_calibration <- function(x, bias, stability, policy) {
  if (is.null(policy)) {
    return(list(
      decision = "NOT_ASSESSED",
      checks = assessment_row(
        "acceptance_policy", "NOT_ASSESSED", NA_real_, NA_character_,
        NA_real_,
        "No acceptance policy was supplied; evidence is descriptive only."
      )
    ))
  }
  if (!inherits(policy, "yourhonor_policy")) {
    cli::cli_abort("{.arg policy} must come from {.fn acceptance_policy}.")
  }
  rows <- c(
    calibration_checks(x, policy),
    human_reliability_checks(x, policy),
    class_metric_checks(x, policy),
    probe_checks(x, bias, policy),
    stability_checks(x, stability, policy)
  )
  checks <- vctrs::vec_rbind(!!!rows)
  list(decision = overall_assessment(checks$status), checks = checks)
}

calibration_checks <- function(x, policy) {
  rows <- list(
    compare_min_count("usable_items", x$n_items, policy$min_n),
    confidence_level_check(
      "calibration_confidence_level", x$conf_level, policy$conf_level
    ),
    compare_max_value(
      "calibration_missing_rate", x$n_excluded / x$n_input,
      policy$max_missing_rate,
      "Too many rows lacked a usable judge-consensus pair."
    )
  )
  check <- paste0("agreement_", policy$coefficient)
  threshold <- policy$min_agreement_lower
  ag <- x$agreement[x$agreement$coefficient == policy$coefficient, , drop = FALSE]
  agreement_row <- if (nrow(ag) == 0L) {
    assessment_row(
      check, "NOT_ASSESSED", NA_real_, "lower >=", threshold,
      "Required coefficient was not computed."
    )
  } else if (ag$status[[1]] != "ok" || !isTRUE(ag$ci_reliable[[1]]) ||
             !is.finite(ag$conf_low[[1]]) || !is.finite(ag$conf_high[[1]])) {
    assessment_row(
      check, "INDETERMINATE", ag$estimate[[1]], "lower >=", threshold,
      paste("Agreement evidence is not reliable:", ag$status[[1]])
    )
  } else {
    interval_min_check(check, ag$conf_low[[1]], ag$conf_high[[1]], threshold)
  }
  c(rows, list(agreement_row))
}

human_reliability_checks <- function(x, policy) {
  threshold <- policy$min_human_reliability_lower
  if (is.null(threshold)) return(list())
  coefficient <- if (identical(x$weights, "unweighted")) {
    "gwet_ac1"
  } else {
    "gwet_ac2"
  }
  hr <- x$human_reliability
  row <- if (is.null(hr)) NULL else
    hr[hr$coefficient == coefficient, , drop = FALSE]
  check <- "human_reliability"
  if (is.null(row) || nrow(row) == 0L) {
    return(list(assessment_row(
      check, "NOT_ASSESSED", NA_real_, "lower >=", threshold,
      "The policy requires multiple-human reliability evidence."
    )))
  }
  if (row$status[[1]] != "ok" || !isTRUE(row$ci_reliable[[1]])) {
    return(list(assessment_row(
      check, "INDETERMINATE", row$estimate[[1]], "lower >=", threshold,
      "Human-reliability evidence is unavailable or unreliable."
    )))
  }
  list(interval_min_check(
    check, row$conf_low[[1]], row$conf_high[[1]], threshold
  ))
}

class_metric_checks <- function(x, policy) {
  metrics <- x$confusion_metrics
  weak_support <- metrics$label[metrics$support < policy$min_class_n]
  support_row <- if (length(weak_support) > 0L) {
    assessment_row(
      "class_support", "INDETERMINATE", min(metrics$support), ">=",
      policy$min_class_n,
      paste(
        "Insufficient reference support for:",
        paste(weak_support, collapse = ", ")
      )
    )
  } else {
    assessment_row(
      "class_support", "PASS", min(metrics$support), ">=",
      policy$min_class_n, "All classes meet support."
    )
  }
  rows <- list(
    support_row,
    class_interval_checks(metrics, policy$min_recall, "recall")
  )
  if (!is.null(policy$min_precision)) {
    rows <- c(rows, list(
      class_interval_checks(metrics, policy$min_precision, "precision")
    ))
  }
  rows
}

# One required probe yields up to four checks: `bias_<probe>` (flip-rate
# interval), `bias_<probe>_direction`, `bias_<probe>_missing_rate`, and
# `bias_<probe>_n`. Prerequisite failures are reported under `bias_<probe>`
# with the flip-rate threshold so the check name keeps one meaning.
probe_checks <- function(x, bias, policy) {
  unlist(
    lapply(policy$required_probes, function(probe) {
      single_probe_checks(probe, x, bias, policy)
    }),
    recursive = FALSE
  )
}

single_probe_checks <- function(probe, x, bias, policy) {
  check <- paste0("bias_", probe)
  threshold <- policy$max_probe_flip_rate
  prerequisite <- function(status, reason) {
    list(assessment_row(
      check, status, NA_real_, "upper <=", threshold, reason
    ))
  }
  not_supplied <- "Required bias probe was not supplied."
  if (is.null(bias)) return(prerequisite("NOT_ASSESSED", not_supplied))
  row <- bias[bias$probe == probe, , drop = FALSE]
  if (nrow(row) == 0L) return(prerequisite("NOT_ASSESSED", not_supplied))
  if (!evidence_is_bound(x, bias)) {
    return(prerequisite(
      "NOT_ASSESSED",
      "Bias evidence is not bound to this calibration by a shared audit_id."
    ))
  }
  if (nrow(row) != 1L) {
    return(prerequisite(
      "INDETERMINATE",
      "Multiple rows were supplied for the required bias probe."
    ))
  }
  if (!same_confidence_level(row$conf_level[[1]], policy$conf_level)) {
    return(list(confidence_level_check(
      paste0(check, "_confidence_level"), row$conf_level[[1]],
      policy$conf_level
    )))
  }
  rows <- list(compare_max_value(
    paste0(check, "_missing_rate"), row$missing_rate[[1]],
    policy$max_probe_missing_rate,
    "Too many individual probe judgments were missing."
  ))
  if (!identical(row$status[[1]], "ok")) {
    return(c(rows, prerequisite(
      "INDETERMINATE",
      paste0("Bias probe did not yield usable evidence: ", row$status[[1]], ".")
    )))
  }
  c(rows, list(
    compare_min_count(
      paste0(check, "_n"), row$n_scored[[1]], policy$min_probe_n
    ),
    interval_max_check(
      paste0(check, "_direction"),
      row$directional_conf_low[[1]], row$directional_conf_high[[1]],
      policy$max_probe_directional_rate
    ),
    interval_max_check(check, row$conf_low[[1]], row$conf_high[[1]], threshold)
  ))
}

stability_checks <- function(x, stability, policy) {
  threshold <- policy$max_pairwise_disagreement
  if (is.null(threshold)) return(list())
  check <- "stability_pairwise_disagreement"
  if (is.null(stability)) {
    return(list(assessment_row(
      check, "NOT_ASSESSED", NA_real_, "upper <=", threshold,
      "Required stability evidence was not supplied."
    )))
  }
  if (!evidence_is_bound(x, stability)) {
    return(list(assessment_row(
      check, "NOT_ASSESSED", NA_real_, "upper <=", threshold,
      paste(
        "Stability evidence is not bound to this calibration by a shared",
        "audit_id."
      )
    )))
  }
  disagreement_row <- if (
    !same_confidence_level(stability$conf_level, policy$conf_level)
  ) {
    confidence_level_check(
      "stability_confidence_level", stability$conf_level, policy$conf_level
    )
  } else if (!isTRUE(stability$pairwise_ci_reliable) ||
             !is.finite(stability$pairwise_disagreement_conf_low) ||
             !is.finite(stability$pairwise_disagreement_conf_high)) {
    assessment_row(
      check, "INDETERMINATE", stability$pairwise_disagreement, "upper <=",
      threshold, "Stability interval is unavailable or unreliable."
    )
  } else {
    interval_max_check(
      check,
      stability$pairwise_disagreement_conf_low,
      stability$pairwise_disagreement_conf_high,
      threshold
    )
  }
  list(
    disagreement_row,
    compare_min_count(
      "stability_items", stability$n_items_scored, policy$min_stability_n
    ),
    compare_min_count(
      "stability_runs", stability$n_runs, policy$min_stability_runs
    ),
    compare_max_value(
      "stability_missing_rate", stability$missing_rate,
      policy$max_stability_missing_rate,
      "Too many stability judgments were missing."
    )
  )
}

# Evidence binding ------------------------------------------------------------

# Each evidence class stores its bundle identifier differently (a list element
# for calibration and stability, an attribute for the bias tibble); this
# generic is the one place that knows where.
audit_id <- function(x, ...) {
  UseMethod("audit_id")
}

#' @export
audit_id.yourhonor_calibration <- function(x, ...) {
  x$audit_id
}

#' @export
audit_id.yourhonor_stability <- function(x, ...) {
  x$audit_id
}

#' @export
audit_id.yourhonor_bias <- function(x, ...) {
  attr(x, "audit_id", exact = TRUE)
}

# An identifier binds evidence only when it is a real value on both sides; two
# missing or empty identifiers agree with each other but bind nothing.
is_binding_id <- function(id) {
  is.character(id) && length(id) == 1L && !is.na(id) && nzchar(id)
}

evidence_is_bound <- function(calibration, evidence) {
  calibration_id <- audit_id(calibration)
  evidence_id <- audit_id(evidence)
  is_binding_id(calibration_id) && is_binding_id(evidence_id) &&
    identical(calibration_id, evidence_id)
}

# Check rows ------------------------------------------------------------------

same_confidence_level <- function(actual, required) {
  length(actual) == 1L && is.finite(actual) &&
    isTRUE(all.equal(as.numeric(actual), as.numeric(required), tolerance = 1e-12))
}

confidence_level_check <- function(check, actual, required) {
  if (same_confidence_level(actual, required)) {
    assessment_row(
      check, "PASS", actual, "=", required, "Confidence level matches policy."
    )
  } else {
    assessment_row(
      check, "INDETERMINATE", actual, "=", required,
      "Evidence confidence level does not match the declared policy."
    )
  }
}

assessment_row <- function(check, status, value, op, threshold, reason) {
  tibble::tibble(
    check = check,
    status = status,
    value = as.numeric(value),
    op = as.character(op),
    threshold = as.numeric(threshold),
    reason = reason
  )
}

compare_min_count <- function(check, value, threshold) {
  if (value >= threshold) {
    assessment_row(
      check, "PASS", value, ">=", threshold, "Sample size is sufficient."
    )
  } else {
    assessment_row(
      check, "INDETERMINATE", value, ">=", threshold,
      "Sample size is below the declared minimum."
    )
  }
}

compare_max_value <- function(check, value, threshold, fail_reason) {
  if (is.finite(value) && value <= threshold) {
    assessment_row(check, "PASS", value, "<=", threshold, "Criterion met.")
  } else {
    assessment_row(check, "FAIL", value, "<=", threshold, fail_reason)
  }
}

interval_min_check <- function(check, low, high, threshold) {
  op <- "lower >="
  if (!is.finite(low) || !is.finite(high)) {
    return(assessment_row(
      check, "INDETERMINATE", NA_real_, op, threshold,
      "Interval is unavailable."
    ))
  }
  if (low >= threshold) {
    assessment_row(
      check, "PASS", low, op, threshold, "Interval clears threshold."
    )
  } else if (high < threshold) {
    assessment_row(
      check, "FAIL", low, op, threshold, "Interval is below threshold."
    )
  } else {
    assessment_row(
      check, "INDETERMINATE", low, op, threshold,
      "Interval overlaps threshold."
    )
  }
}

interval_max_check <- function(check, low, high, threshold) {
  op <- "upper <="
  if (!is.finite(low) || !is.finite(high)) {
    return(assessment_row(
      check, "INDETERMINATE", NA_real_, op, threshold,
      "Interval is unavailable."
    ))
  }
  if (high <= threshold) {
    assessment_row(
      check, "PASS", high, op, threshold, "Interval clears threshold."
    )
  } else if (low > threshold) {
    assessment_row(
      check, "FAIL", high, op, threshold, "Interval exceeds threshold."
    )
  } else {
    assessment_row(
      check, "INDETERMINATE", high, op, threshold,
      "Interval overlaps threshold."
    )
  }
}

class_interval_checks <- function(metrics, threshold, metric) {
  labels <- metrics$label
  if (length(threshold) == 1L && is.null(names(threshold))) {
    thresholds <- rep(threshold, length(labels))
    names(thresholds) <- labels
  } else {
    thresholds <- threshold
  }
  rows <- lapply(names(thresholds), function(label) {
    i <- match(label, labels)
    check <- paste0(metric, "_", label)
    bound <- thresholds[[label]]
    if (is.na(i)) {
      return(assessment_row(
        check, "NOT_ASSESSED", NA_real_, "lower >=", bound,
        "Policy names a class absent from the calibration label universe."
      ))
    }
    low <- metrics[[paste0(metric, "_conf_low")]][[i]]
    high <- metrics[[paste0(metric, "_conf_high")]][[i]]
    if (!is.finite(low) || !is.finite(high)) {
      return(assessment_row(
        check, "INDETERMINATE", metrics[[metric]][[i]], "lower >=", bound,
        "Class interval is undefined."
      ))
    }
    interval_min_check(check, low, high, bound)
  })
  vctrs::vec_rbind(!!!rows)
}

overall_assessment <- function(status) {
  if (any(status == "FAIL")) return("FAIL")
  if (any(status == "NOT_ASSESSED")) return("NOT_ASSESSED")
  if (any(status == "INDETERMINATE")) return("INDETERMINATE")
  "PASS"
}
