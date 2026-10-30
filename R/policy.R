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
#'   selected agreement coefficient. The bound assessed is the smaller of the
#'   bootstrap `conf_low` and the exact `conf_low_exact` reported by
#'   [agreement()]. On a perfect sample the bootstrap interval is a
#'   zero-width point at 1 and only `conf_low_exact` is used, so a perfect
#'   sample is assessed on the same footing as a near-perfect one rather than
#'   reading `INDETERMINATE` where a single error could `PASS`. Comparing a
#'   two-sided interval's lower bound with the threshold is a one-sided test
#'   at level `(1 + conf_level) / 2`, and taking the smaller of two lower
#'   bounds makes it more conservative than that.
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
#'   probe's flip rate. The upper bound assessed counts every unscored item
#'   (one without a usable judgment in both conditions; for the position
#'   probe this includes items lost to a within-condition tie) as a flip over
#'   all `n_input` items, so it can exceed the probe's own `conf_high`; the
#'   lower bound is the scored items' `conf_low`. Counting them can turn this
#'   check's `PASS` into `INDETERMINATE` but never into `FAIL`. Required when
#'   `required_probes` is non-empty and rejected otherwise.
#' @param max_probe_directional_rate Maximum practically tolerable net
#'   directional effect, assessed against a conservative confidence interval
#'   whose upper bound counts every unscored item towards the worse direction,
#'   as for `max_probe_flip_rate`. Required when `required_probes` is
#'   non-empty and rejected otherwise.
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
#' @param coefficient Agreement coefficient used by the policy. `NULL` (the
#'   default) selects the calibration's Gwet coefficient: `"gwet_ac1"` for an
#'   unweighted (nominal) calibration and `"gwet_ac2"` for a weighted
#'   (ordinal) one. When the calibration did not compute that default
#'   coefficient, the agreement check is `NOT_ASSESSED` with the reason, and
#'   the other checks are still assessed. A coefficient named here that the
#'   calibration did not compute is an error when the policy is assessed,
#'   never a silent `NOT_ASSESSED`.
#' @param max_abstention_rate Optional maximum upper confidence bound for the
#'   judge's abstention rate: the share of items with a human reference that
#'   the judge left `NA`. See the "Missing labels and abstentions" section of
#'   [calibrate_judge()].
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
                              coefficient = NULL,
                              max_abstention_rate = NULL) {
  # Every validation error acceptance_policy() raises carries `policy_error`,
  # so a caller can catch a malformed policy as one condition class; the
  # rlang argument checks are re-signalled under it with the original as
  # `parent`.
  call <- rlang::current_env()
  coefficient <- rlang::try_fetch(
    {
      rlang::check_required(min_agreement_lower)
      rlang::check_required(min_n)
      rlang::check_required(min_class_n)
      rlang::check_required(min_recall)
      rlang::check_required(max_missing_rate)
      rlang::check_required(max_pairwise_disagreement)
      if (!is.null(coefficient)) {
        rlang::check_string(coefficient)
        rlang::arg_match(coefficient, supported_coefficients)
      }
    },
    error = function(err) {
      header <- rlang::cnd_header(err)
      cli::cli_abort(
        "{header}", class = policy_error, parent = err, call = call
      )
    }
  )
  conf_level <- check_probability(conf_level, open = TRUE, class = policy_error)
  min_agreement_lower <- check_numeric_threshold(min_agreement_lower)
  min_n <- check_count(min_n, minimum = 2L, class = policy_error)
  min_class_n <- check_count(min_class_n, class = policy_error)
  min_recall <- check_metric_threshold(min_recall)
  max_missing_rate <- check_probability(max_missing_rate, class = policy_error)
  min_precision <- check_optional(min_precision, check_metric_threshold)
  min_human_reliability_lower <- check_optional(
    min_human_reliability_lower, check_numeric_threshold
  )
  max_abstention_rate <- check_optional(
    max_abstention_rate, check_probability, class = policy_error
  )

  max_pairwise_disagreement <- check_optional(
    max_pairwise_disagreement, check_probability, class = policy_error
  )
  check_dependent_thresholds(
    list(
      min_stability_n = min_stability_n,
      min_stability_runs = min_stability_runs,
      max_stability_missing_rate = max_stability_missing_rate
    ),
    gate_on = !is.null(max_pairwise_disagreement),
    gate = "max_pairwise_disagreement",
    gate_on_state = "is not NULL",
    gate_off_state = "is NULL"
  )
  min_stability_n <- check_optional(
    min_stability_n, check_count, class = policy_error
  )
  min_stability_runs <- check_optional(
    min_stability_runs, check_count, minimum = 2L, class = policy_error
  )
  max_stability_missing_rate <- check_optional(
    max_stability_missing_rate, check_probability, class = policy_error
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
    gate_on_state = "is non-empty",
    gate_off_state = "is empty"
  )
  min_probe_n <- check_optional(min_probe_n, check_count, class = policy_error)
  max_probe_flip_rate <- check_optional(
    max_probe_flip_rate, check_probability, class = policy_error
  )
  max_probe_directional_rate <- check_optional(
    max_probe_directional_rate, check_probability, class = policy_error
  )
  max_probe_missing_rate <- check_optional(
    max_probe_missing_rate, check_probability, class = policy_error
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
      max_abstention_rate = max_abstention_rate,
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

# Condition class shared by every error acceptance_policy() raises.
policy_error <- "yourhonor_policy_error"

check_required_probes <- function(x,
                                  arg = rlang::caller_arg(x),
                                  call = rlang::caller_env()) {
  if (!is.character(x) || anyNA(x) || any(!nzchar(x))) {
    cli::cli_abort(
      "{.arg {arg}} must be a character vector of probe names.",
      class = policy_error, call = call
    )
  }
  unknown <- setdiff(x, bias_probe_names)
  if (length(unknown) > 0L) {
    cli::cli_abort(
      c(
        "Unknown {.arg {arg}}: {.val {unknown}}.",
        i = "Known probes: {.val {bias_probe_names}}."
      ),
      class = policy_error, call = call
    )
  }
  if (anyDuplicated(x)) {
    cli::cli_abort(
      paste0(
        "{.arg {arg}} must not repeat a probe: ",
        "{.val {unique(x[duplicated(x)])}}."
      ),
      class = policy_error, call = call
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
                                       gate_on_state,
                                       gate_off_state,
                                       call = rlang::caller_env()) {
  supplied <- !vapply(thresholds, is.null, logical(1))
  args <- names(thresholds)
  if (gate_on && !all(supplied)) {
    cli::cli_abort(
      paste0(
        "{.arg {args[!supplied]}} {?is/are} required when ",
        "{.arg {gate}} {gate_on_state}."
      ),
      class = policy_error, call = call
    )
  }
  if (!gate_on && any(supplied)) {
    # `qty()` restores the count after `{gate}` (length one) has reset cli's
    # pluralisation quantity.
    cli::cli_abort(
      c(
        paste0(
          "{.arg {args[supplied]}} {?was/were} supplied but ",
          "{.arg {gate}} {gate_off_state}."
        ),
        i = paste0(
          "{cli::qty(sum(supplied))}{?This threshold is/These thresholds ",
          "are} read only when {.arg {gate}} {gate_on_state}; a threshold ",
          "behind an inactive gate is never assessed."
        ),
        i = paste0(
          "Either drop {cli::qty(sum(supplied))}{?it/them} or supply ",
          "{.arg {gate}}."
        )
      ),
      class = policy_error, call = call
    )
  }
  invisible(NULL)
}

check_numeric_threshold <- function(x,
                                    arg = rlang::caller_arg(x),
                                    call = rlang::caller_env()) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      x < -1 || x > 1) {
    cli::cli_abort(
      "{.arg {arg}} must be one finite number between -1 and 1.",
      class = policy_error, call = call
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
      class = policy_error, call = call
    )
  }
  x
}

# Assessment ------------------------------------------------------------------

# Every check row carries the comparison it encodes (`op` and a numeric
# `threshold`) next to the observed `value`, so renderers and the JSON
# artifact can show a failure's magnitude without parsing prose. `value` is
# always the quantity `op` compares (a count, a rate, or the interval bound
# named by `op`) and is `NA` whenever that quantity is unavailable or not
# trusted, so a rendered line never shows a number the check did not use.
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
  # Interval checks are only meaningful at the policy's confidence level;
  # counts and rates do not depend on it and are assessed regardless.
  level_ok <- same_confidence_level(x$conf_level, policy$conf_level)
  rows <- c(
    calibration_checks(x, policy, level_ok),
    human_reliability_checks(x, policy, level_ok),
    class_metric_checks(x, policy, level_ok),
    probe_checks(x, bias, policy),
    stability_checks(x, stability, policy)
  )
  checks <- vctrs::vec_rbind(!!!rows)
  list(decision = overall_assessment(checks$status), checks = checks)
}

level_mismatch_reason <- paste(
  "Calibration confidence level does not match the policy;",
  "the interval was not assessed."
)

# The coefficient a policy assesses on `x`: its own choice, or by default
# the Gwet coefficient matching the calibration's weights. An explicit choice
# the calibration did not compute is a configuration error, not missing
# evidence, so it stops the assessment. The default is the policy's choice
# rather than the user's, so when the calibration lacks it the agreement
# check reads `NOT_ASSESSED` and the rest of the report still stands.
policy_coefficient <- function(policy, x, call = rlang::caller_env()) {
  coefficient <- policy$coefficient
  if (is.null(coefficient)) return(calibration_gwet(x))
  computed <- x$agreement$coefficient
  if (!coefficient %in% computed) {
    cli::cli_abort(
      c(
        "The policy assesses {.val {coefficient}}, which this calibration did not compute.",
        i = "Computed: {.val {computed}}.",
        i = paste0(
          "Request {.val {coefficient}} in {.fn calibrate_judge}, or name ",
          "a computed coefficient in {.fn acceptance_policy}. A ",
          "{.code NULL} {.arg coefficient} assesses the calibration's ",
          "Gwet coefficient."
        )
      ),
      call = call
    )
  }
  coefficient
}

# Gwet's coefficient under the calibration's weights; a calibration that
# records no weights is unweighted.
calibration_gwet <- function(x) {
  if (is.null(x$weights) || identical(x$weights, "unweighted")) {
    "gwet_ac1"
  } else {
    "gwet_ac2"
  }
}

calibration_checks <- function(x, policy, level_ok) {
  coefficient <- policy_coefficient(policy, x)
  rows <- list(
    compare_min_count("usable_items", x$n_items, policy$min_n),
    confidence_level_check(
      "calibration_confidence_level", x$conf_level, policy$conf_level
    ),
    compare_max_value(
      "calibration_missing_rate", x$n_excluded / x$n_input,
      policy$max_missing_rate,
      "Too many rows lacked a usable judge-consensus pair."
    ),
    coefficient_interval_check(
      paste0("agreement_", coefficient), x$agreement,
      coefficient, policy$min_agreement_lower, level_ok,
      absent = paste0(
        "The calibration did not compute ", coefficient, ", the policy's ",
        "default coefficient; request it in calibrate_judge() or name a ",
        "computed coefficient in acceptance_policy()."
      ),
      what = "Agreement"
    )
  )
  if (!is.null(policy$max_abstention_rate)) {
    rows <- c(rows, list(abstention_check(x, policy, level_ok)))
  }
  rows
}

abstention_check <- function(x, policy, level_ok) {
  check <- "judge_abstention_rate"
  threshold <- policy$max_abstention_rate
  if (is.null(x$judge_abstention_conf_high)) {
    return(assessment_row(
      check, "NOT_ASSESSED", NA_real_, "upper <=", threshold,
      "The calibration does not record judge abstentions."
    ))
  }
  if (!level_ok) {
    return(assessment_row(
      check, "INDETERMINATE", NA_real_, "upper <=", threshold,
      level_mismatch_reason
    ))
  }
  interval_max_check(
    check, x$judge_abstention_conf_low, x$judge_abstention_conf_high,
    threshold
  )
}

human_reliability_checks <- function(x, policy, level_ok) {
  threshold <- policy$min_human_reliability_lower
  if (is.null(threshold)) return(list())
  coefficient <- calibration_gwet(x)
  list(coefficient_interval_check(
    "human_reliability", x$human_reliability, coefficient, threshold,
    level_ok,
    absent = "The policy requires multiple-human reliability evidence.",
    what = "Human-reliability"
  ))
}

# One `lower >=` check on a coefficient row of an agreement table. The row
# must exist exactly once and carry a trusted interval at the policy's
# confidence level before its lower bound is compared.
coefficient_interval_check <- function(check,
                                       table,
                                       coefficient,
                                       threshold,
                                       level_ok,
                                       absent,
                                       what) {
  op <- "lower >="
  row <- if (is.null(table)) {
    NULL
  } else {
    table[table$coefficient == coefficient, , drop = FALSE]
  }
  if (is.null(row) || nrow(row) == 0L) {
    return(assessment_row(
      check, "NOT_ASSESSED", NA_real_, op, threshold, absent
    ))
  }
  if (nrow(row) > 1L) {
    return(assessment_row(
      check, "INDETERMINATE", NA_real_, op, threshold,
      paste0("Multiple rows were supplied for coefficient ", coefficient, ".")
    ))
  }
  if (!level_ok) {
    return(assessment_row(
      check, "INDETERMINATE", NA_real_, op, threshold, level_mismatch_reason
    ))
  }
  exact_low <- if ("conf_low_exact" %in% names(row)) {
    row$conf_low_exact[[1]]
  } else {
    NA_real_
  }
  # The bound assessed is the smaller of the bootstrap and exact lower bounds
  # on every row. A perfect sample's bootstrap is a zero-width point at 1
  # that says nothing about the population: it is not trusted on its own,
  # but with an exact bound beside it the minimum is that exact bound, and
  # the interval runs up to the coefficient's maximum of 1.
  degenerate <- isTRUE(row$boundary_degenerate[[1]])
  exact_bounded <- degenerate && is.finite(exact_low) &&
    identical(row$status[[1]], "ok")
  unreliable <- interval_unreliable_reason(row, what)
  if (!is.null(unreliable) && !exact_bounded) {
    return(assessment_row(
      check, "INDETERMINATE", NA_real_, op, threshold, unreliable
    ))
  }
  unusable <- interval_unusable_reason(row$conf_low[[1]], row$conf_high[[1]])
  if (!is.null(unusable)) {
    return(assessment_row(
      check, "INDETERMINATE", NA_real_, op, threshold, unusable
    ))
  }
  low <- min(row$conf_low[[1]], exact_low, na.rm = TRUE)
  high <- if (degenerate) 1 else row$conf_high[[1]]
  result <- interval_min_check(check, low, high, threshold)
  if (is.finite(exact_low) && low == exact_low &&
      !identical(low, row$conf_low[[1]])) {
    result$reason <- paste(
      result$reason,
      "The lower bound is conf_low_exact (the exact bound on observed",
      "agreement mapped onto the coefficient's scale, capped at the bound of",
      "the perfect samples the ratings can be corrected into)."
    )
  }
  result
}

# NULL when an agreement row's interval can be compared with a threshold;
# otherwise the specific reason it cannot, so the reader learns the cause
# rather than an echo of `status = "ok"`.
interval_unreliable_reason <- function(row, what) {
  status <- row$status[[1]]
  if (!identical(status, "ok")) {
    return(paste0(what, " is not estimable: ", status, "."))
  }
  if (isTRUE(row$boundary_degenerate[[1]])) {
    return(paste0(
      what, " bootstrap interval is degenerate at the boundary and is not ",
      "trusted."
    ))
  }
  if (!isTRUE(row$ci_reliable[[1]])) {
    return(paste0(
      what, " bootstrap interval is unreliable: too few finite resamples."
    ))
  }
  if (!is.finite(row$conf_low[[1]]) || !is.finite(row$conf_high[[1]])) {
    return(paste0(what, " interval is unavailable."))
  }
  NULL
}

class_metric_checks <- function(x, policy, level_ok) {
  metrics <- x$confusion_metrics
  if (is.null(metrics) || nrow(metrics) == 0L) {
    # `min()` over no support would be `Inf` and read as a pass.
    return(list(assessment_row(
      "class_support", "NOT_ASSESSED", NA_real_, ">=", policy$min_class_n,
      "No per-class metrics are available."
    )))
  }
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
    class_interval_checks(metrics, policy$min_recall, "recall", level_ok)
  )
  if (!is.null(policy$min_precision)) {
    rows <- c(rows, list(class_interval_checks(
      metrics, policy$min_precision, "precision", level_ok
    )))
  }
  rows
}

# One required probe yields up to four checks: `bias_<probe>` (flip-rate
# interval), `bias_<probe>_direction`, `bias_<probe>_missing_rate`, and
# `bias_<probe>_n`. The status and sample-size gates are prerequisites for
# the two interval checks: when one stops the assessment the outcome is
# reported under `bias_<probe>` with the flip-rate threshold so that check
# name keeps one meaning. The missingness gate is an independent check: it
# can `FAIL` the policy on its own, and the interval checks are still
# assessed. Their lower bounds are the scored items' own; their upper bounds
# count every unscored item as a flip (worst_case_upper_bounds()), so items
# lost to abstention or position ties can widen a bound but never clear it.
# A probe at another confidence level reports
# `bias_<probe>_confidence_level` in place of the interval checks while its
# rate and count checks are still assessed.
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
  n_row <- compare_min_count(
    paste0(check, "_n"), row$n_scored[[1]], policy$min_probe_n
  )
  rows <- c(rows, list(n_row))
  if (!same_confidence_level(row$conf_level[[1]], policy$conf_level)) {
    return(c(rows, list(confidence_level_check(
      paste0(check, "_confidence_level"), row$conf_level[[1]],
      policy$conf_level
    ))))
  }
  if (!identical(n_row$status, "PASS")) {
    # `min_probe_n` is the policy author's floor on what counts as evidence,
    # so a smaller probe cannot fail (or pass) the interval gates.
    return(c(rows, prerequisite(
      "INDETERMINATE",
      paste(
        "Sample size is below the declared minimum;",
        "the flip-rate interval was not assessed."
      )
    )))
  }
  upper <- worst_case_upper_bounds(row)
  interval_rows <- list(
    interval_max_check(
      paste0(check, "_direction"),
      row$directional_conf_low[[1]], upper$directional,
      policy$max_probe_directional_rate
    ),
    interval_max_check(check, row$conf_low[[1]], upper$flip, threshold)
  )
  n_unscored <- row$n_unscored[[1]]
  if (is.finite(upper$flip) && n_unscored > 0L) {
    items <- paste0(
      n_unscored, " unscored item", if (n_unscored == 1L) "" else "s"
    )
    counted_as <- c(
      paste("towards the worse direction over all", row$n_input[[1]], "items."),
      paste("as flips over all", row$n_input[[1]], "items.")
    )
    for (i in seq_along(interval_rows)) {
      interval_rows[[i]]$reason <- paste(
        interval_rows[[i]]$reason, "The upper bound counts the", items,
        counted_as[[i]]
      )
    }
  }
  c(rows, interval_rows)
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
      check, "INDETERMINATE", NA_real_, "upper <=", threshold,
      "Stability interval is unavailable or unreliable."
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
    value = scalar_number(value),
    op = as.character(op),
    threshold = scalar_number(threshold),
    reason = reason
  )
}

# A NULL or NA measurement is one missing number, never a zero-length column
# that would silently drop the whole row from the checks table.
scalar_number <- function(x) {
  if (is.null(x) || length(x) == 0L) return(NA_real_)
  if (length(x) != 1L) {
    cli::cli_abort("Internal error: a check value must be a single number.")
  }
  as.numeric(x)
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

# NULL when `[low, high]` is a usable interval; otherwise why it is not. A
# reversed interval is malformed evidence, not a narrow one.
interval_unusable_reason <- function(low, high) {
  if (!is.finite(low) || !is.finite(high)) return("Interval is unavailable.")
  if (low > high) return("Interval bounds are reversed.")
  NULL
}

interval_min_check <- function(check, low, high, threshold) {
  op <- "lower >="
  unusable <- interval_unusable_reason(low, high)
  if (!is.null(unusable)) {
    return(assessment_row(
      check, "INDETERMINATE", NA_real_, op, threshold, unusable
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
  unusable <- interval_unusable_reason(low, high)
  if (!is.null(unusable)) {
    return(assessment_row(
      check, "INDETERMINATE", NA_real_, op, threshold, unusable
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

class_interval_checks <- function(metrics, threshold, metric, level_ok) {
  labels <- metrics$label
  if (length(threshold) == 1L && is.null(names(threshold))) {
    thresholds <- rep(threshold, length(labels))
    names(thresholds) <- labels
  } else {
    thresholds <- threshold
  }
  op <- "lower >="
  rows <- lapply(names(thresholds), function(label) {
    i <- match(label, labels)
    check <- paste0(metric, "_", label)
    bound <- thresholds[[label]]
    if (is.na(i)) {
      return(assessment_row(
        check, "NOT_ASSESSED", NA_real_, op, bound,
        "Policy names a class absent from the calibration label universe."
      ))
    }
    if (!level_ok) {
      return(assessment_row(
        check, "INDETERMINATE", NA_real_, op, bound, level_mismatch_reason
      ))
    }
    low <- metrics[[paste0(metric, "_conf_low")]][[i]]
    high <- metrics[[paste0(metric, "_conf_high")]][[i]]
    if (!is.finite(low) || !is.finite(high)) {
      return(assessment_row(
        check, "INDETERMINATE", NA_real_, op, bound,
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
