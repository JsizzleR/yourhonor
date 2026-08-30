#' Declare a judge acceptance policy
#'
#' A policy turns descriptive calibration evidence into a decision. Thresholds
#' are intentionally supplied by the caller because acceptable error depends on
#' the task and the cost of each class of mistake.
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
#'   judgments. Required when `max_pairwise_disagreement` is not `NULL`.
#' @param min_stability_runs Minimum repeated judge runs. Required when
#'   `max_pairwise_disagreement` is not `NULL`.
#' @param max_stability_missing_rate Maximum fraction of individual repeated
#'   judgments that may be missing. Required when stability is in the policy.
#' @param required_probes Bias probes that must be present and free of detected
#'   signal.
#' @param min_probe_n Minimum paired comparisons required for every required
#'   bias probe. Required when `required_probes` is non-empty.
#' @param max_probe_flip_rate Maximum upper confidence bound for a required
#'   probe's flip rate. Required when `required_probes` is non-empty.
#' @param max_probe_directional_rate Maximum practically tolerable net
#'   directional effect, assessed against a conservative confidence interval.
#' @param max_probe_missing_rate Maximum fraction of individual probe judgments
#'   that may be missing. Required when `required_probes` is non-empty so
#'   selective abstention cannot make a probe look clean.
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
  coefficient <- rlang::arg_match(coefficient)
  conf_level <- check_probability(conf_level, "conf_level", open = TRUE)
  min_agreement_lower <- check_numeric_threshold(min_agreement_lower, "min_agreement_lower")
  min_n <- check_count(min_n, "min_n", minimum = 2L)
  min_class_n <- check_count(min_class_n, "min_class_n", minimum = 1L)
  min_recall <- check_metric_threshold(min_recall, "min_recall")
  if (!is.null(min_precision)) {
    min_precision <- check_metric_threshold(min_precision, "min_precision")
  }
  if (!is.null(min_human_reliability_lower)) {
    min_human_reliability_lower <- check_numeric_threshold(
      min_human_reliability_lower, "min_human_reliability_lower"
    )
  }
  max_missing_rate <- check_probability(max_missing_rate, "max_missing_rate")
  if (!is.null(max_pairwise_disagreement)) {
    max_pairwise_disagreement <- check_probability(
      max_pairwise_disagreement, "max_pairwise_disagreement"
    )
    if (is.null(min_stability_n) || is.null(min_stability_runs) ||
        is.null(max_stability_missing_rate)) {
      cli::cli_abort(paste0(
        "{.arg min_stability_n}, {.arg min_stability_runs}, and ",
        "{.arg max_stability_missing_rate} are required when ",
        "{.arg max_pairwise_disagreement} is not NULL."
      ))
    }
    min_stability_n <- check_count(min_stability_n, "min_stability_n")
    min_stability_runs <- check_count(
      min_stability_runs, "min_stability_runs", minimum = 2L
    )
    max_stability_missing_rate <- check_probability(
      max_stability_missing_rate, "max_stability_missing_rate"
    )
  } else {
    if (!is.null(min_stability_n)) {
      min_stability_n <- check_count(min_stability_n, "min_stability_n")
    }
    if (!is.null(min_stability_runs)) {
      min_stability_runs <- check_count(
        min_stability_runs, "min_stability_runs", minimum = 2L
      )
    }
    if (!is.null(max_stability_missing_rate)) {
      max_stability_missing_rate <- check_probability(
        max_stability_missing_rate, "max_stability_missing_rate"
      )
    }
  }
  if (!is.character(required_probes) || anyNA(required_probes) ||
      any(!nzchar(required_probes))) {
    cli::cli_abort("{.arg required_probes} must be a character vector of probe names.")
  }
  allowed_probes <- c("verbosity", "self_preference", "bandwagon", "position")
  unknown <- setdiff(required_probes, allowed_probes)
  if (length(unknown) > 0L || anyDuplicated(required_probes)) {
    cli::cli_abort("Unknown or duplicated {.arg required_probes}: {.val {unknown}}.")
  }
  if (length(required_probes) > 0L) {
    if (is.null(min_probe_n) || is.null(max_probe_flip_rate) ||
        is.null(max_probe_directional_rate) ||
        is.null(max_probe_missing_rate)) {
      cli::cli_abort(paste0(
        "{.arg min_probe_n}, {.arg max_probe_flip_rate}, ",
        "{.arg max_probe_directional_rate}, and ",
        "{.arg max_probe_missing_rate} are required when ",
        "{.arg required_probes} is non-empty."
      ))
    }
    min_probe_n <- check_count(min_probe_n, "min_probe_n")
    max_probe_flip_rate <- check_probability(
      max_probe_flip_rate, "max_probe_flip_rate"
    )
    max_probe_directional_rate <- check_probability(
      max_probe_directional_rate, "max_probe_directional_rate"
    )
    max_probe_missing_rate <- check_probability(
      max_probe_missing_rate, "max_probe_missing_rate"
    )
  } else {
    if (!is.null(min_probe_n)) min_probe_n <- check_count(min_probe_n, "min_probe_n")
    if (!is.null(max_probe_flip_rate)) {
      max_probe_flip_rate <- check_probability(
        max_probe_flip_rate, "max_probe_flip_rate"
      )
    }
    if (!is.null(max_probe_directional_rate)) {
      max_probe_directional_rate <- check_probability(
        max_probe_directional_rate, "max_probe_directional_rate"
      )
    }
    if (!is.null(max_probe_missing_rate)) {
      max_probe_missing_rate <- check_probability(
        max_probe_missing_rate, "max_probe_missing_rate"
      )
    }
  }

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

check_numeric_threshold <- function(x, arg) {
  if (length(x) != 1L || is.na(x) || !is.numeric(x) || !is.finite(x) || x < -1 || x > 1) {
    cli::cli_abort("{.arg {arg}} must be one finite number between -1 and 1.")
  }
  as.numeric(x)
}

check_metric_threshold <- function(x, arg) {
  names_invalid <- !is.null(names(x)) &&
    (anyNA(names(x)) || any(!nzchar(names(x))) || anyDuplicated(names(x)))
  if (!is.numeric(x) || length(x) < 1L || anyNA(x) || any(!is.finite(x)) ||
      any(x < 0 | x > 1) || (length(x) > 1L && is.null(names(x))) ||
      names_invalid) {
    cli::cli_abort(
      "{.arg {arg}} must be a scalar or fully named numeric vector between 0 and 1."
    )
  }
  x
}

assess_calibration <- function(x, bias, stability, policy) {
  if (is.null(policy)) {
    return(list(
      decision = "NOT_ASSESSED",
      checks = assessment_row(
        "acceptance_policy", "NOT_ASSESSED", NA_real_, NA_character_,
        "No acceptance policy was supplied; evidence is descriptive only."
      )
    ))
  }
  if (!inherits(policy, "yourhonor_policy")) {
    cli::cli_abort("{.arg policy} must come from {.fn acceptance_policy}.")
  }

  checks <- list()
  add <- function(row) checks[[length(checks) + 1L]] <<- row

  add(compare_min_count("usable_items", x$n_items, policy$min_n))
  add(confidence_level_check(
    "calibration_confidence_level", x$conf_level, policy$conf_level
  ))
  missing_rate <- x$n_excluded / x$n_input
  add(compare_max_value(
    "calibration_missing_rate", missing_rate, policy$max_missing_rate,
    "Too many rows lacked a usable judge-consensus pair."
  ))

  ag <- x$agreement[x$agreement$coefficient == policy$coefficient, , drop = FALSE]
  if (nrow(ag) == 0L) {
    add(assessment_row(
      paste0("agreement_", policy$coefficient), "NOT_ASSESSED", NA_real_,
      as.character(policy$min_agreement_lower), "Required coefficient was not computed."
    ))
  } else if (ag$status[[1]] != "ok" || !isTRUE(ag$ci_reliable[[1]]) ||
             !is.finite(ag$conf_low[[1]]) || !is.finite(ag$conf_high[[1]])) {
    add(assessment_row(
      paste0("agreement_", policy$coefficient), "INDETERMINATE", ag$estimate[[1]],
      paste0("lower >= ", policy$min_agreement_lower),
      paste("Agreement evidence is not reliable:", ag$status[[1]])
    ))
  } else {
    add(interval_min_check(
      paste0("agreement_", policy$coefficient), ag$conf_low[[1]], ag$conf_high[[1]],
      policy$min_agreement_lower
    ))
  }

  if (!is.null(policy$min_human_reliability_lower)) {
    human_coefficient <- if (identical(x$weights, "unweighted")) {
      "gwet_ac1"
    } else {
      "gwet_ac2"
    }
    hr <- x$human_reliability
    row <- if (is.null(hr)) NULL else
      hr[hr$coefficient == human_coefficient, , drop = FALSE]
    if (is.null(row) || nrow(row) == 0L) {
      add(assessment_row(
        "human_reliability", "NOT_ASSESSED", NA_real_,
        paste0("lower >= ", policy$min_human_reliability_lower),
        "The policy requires multiple-human reliability evidence."
      ))
    } else if (row$status[[1]] != "ok" || !isTRUE(row$ci_reliable[[1]])) {
      add(assessment_row(
        "human_reliability", "INDETERMINATE", row$estimate[[1]],
        paste0("lower >= ", policy$min_human_reliability_lower),
        "Human-reliability evidence is unavailable or unreliable."
      ))
    } else {
      add(interval_min_check(
        "human_reliability", row$conf_low[[1]], row$conf_high[[1]],
        policy$min_human_reliability_lower
      ))
    }
  }

  metrics <- x$confusion_metrics
  weak_support <- metrics$label[metrics$support < policy$min_class_n]
  if (length(weak_support) > 0L) {
    add(assessment_row(
      "class_support", "INDETERMINATE", min(metrics$support),
      paste0("every class >= ", policy$min_class_n),
      paste("Insufficient reference support for:", paste(weak_support, collapse = ", "))
    ))
  } else {
    add(assessment_row(
      "class_support", "PASS", min(metrics$support),
      paste0("every class >= ", policy$min_class_n), "All classes meet support."
    ))
  }
  add(class_interval_checks(metrics, policy$min_recall, "recall"))
  if (!is.null(policy$min_precision)) {
    add(class_interval_checks(metrics, policy$min_precision, "precision"))
  }

  for (probe in policy$required_probes) {
    row <- if (is.null(bias)) NULL else bias[bias$probe == probe, , drop = FALSE]
    if (is.null(bias) || nrow(row) == 0L) {
      add(assessment_row(
        paste0("bias_", probe), "NOT_ASSESSED", NA_real_, "no detected bias",
        "Required bias probe was not supplied."
      ))
    } else if (!evidence_is_bound(x, bias)) {
      add(assessment_row(
        paste0("bias_", probe), "NOT_ASSESSED", NA_real_, "bound audit evidence",
        "Bias evidence is not bound to this calibration by a shared audit_id."
      ))
    } else if (nrow(row) != 1L) {
      add(assessment_row(
        paste0("bias_", probe), "INDETERMINATE", NA_real_, "one probe result",
        "Multiple rows were supplied for the required bias probe."
      ))
    } else if (!same_confidence_level(row$conf_level[[1]], policy$conf_level)) {
      add(confidence_level_check(
        paste0("bias_", probe, "_confidence_level"),
        row$conf_level[[1]], policy$conf_level
      ))
    } else {
      add(compare_max_value(
        paste0("bias_", probe, "_missing_rate"), row$missing_rate[[1]],
        policy$max_probe_missing_rate,
        "Too many individual probe judgments were missing."
      ))
      if (!identical(row$status[[1]], "ok")) {
        add(assessment_row(
          paste0("bias_", probe), "INDETERMINATE", row$flip_rate[[1]], "valid probe",
          "Bias probe did not yield usable evidence."
        ))
      } else if (row$n_scored[[1]] < policy$min_probe_n) {
        add(assessment_row(
          paste0("bias_", probe), "INDETERMINATE", row$n_scored[[1]],
          paste0(">= ", policy$min_probe_n),
          "Bias probe sample size is below the declared minimum."
        ))
      } else {
        add(interval_max_check(
          paste0("bias_", probe, "_direction"),
          row$directional_conf_low[[1]], row$directional_conf_high[[1]],
          policy$max_probe_directional_rate
        ))
        add(interval_max_check(
          paste0("bias_", probe), row$conf_low[[1]], row$conf_high[[1]],
          policy$max_probe_flip_rate
        ))
      }
    }
  }

  if (!is.null(policy$max_pairwise_disagreement)) {
    if (is.null(stability)) {
      add(assessment_row(
        "stability_pairwise_disagreement", "NOT_ASSESSED", NA_real_,
        paste0("upper <= ", policy$max_pairwise_disagreement),
        "Required stability evidence was not supplied."
      ))
    } else if (!evidence_is_bound(x, stability)) {
      add(assessment_row(
        "stability_pairwise_disagreement", "NOT_ASSESSED", NA_real_,
        paste0("upper <= ", policy$max_pairwise_disagreement),
        "Stability evidence is not bound to this calibration by a shared audit_id."
      ))
    } else if (!same_confidence_level(stability$conf_level, policy$conf_level)) {
      add(confidence_level_check(
        "stability_confidence_level", stability$conf_level, policy$conf_level
      ))
    } else if (!isTRUE(stability$pairwise_ci_reliable) ||
               !is.finite(stability$pairwise_disagreement_conf_low) ||
               !is.finite(stability$pairwise_disagreement_conf_high)) {
      add(assessment_row(
        "stability_pairwise_disagreement", "INDETERMINATE",
        stability$pairwise_disagreement,
        paste0("upper <= ", policy$max_pairwise_disagreement),
        "Stability interval is unavailable or unreliable."
      ))
    } else {
      add(interval_max_check(
        "stability_pairwise_disagreement",
        stability$pairwise_disagreement_conf_low,
        stability$pairwise_disagreement_conf_high,
        policy$max_pairwise_disagreement
      ))
    }
    if (!is.null(stability) && evidence_is_bound(x, stability)) {
      add(compare_min_count(
        "stability_items", stability$n_items_scored, policy$min_stability_n
      ))
      add(compare_min_count(
        "stability_runs", stability$n_runs, policy$min_stability_runs
      ))
      add(compare_max_value(
        "stability_missing_rate", stability$missing_rate,
        policy$max_stability_missing_rate,
        "Too many stability judgments were missing."
      ))
    }
  }

  checks <- do.call(vctrs::vec_rbind, checks)
  list(decision = overall_assessment(checks$status), checks = checks)
}

evidence_audit_id <- function(x) {
  if (inherits(x, "yourhonor_bias")) return(attr(x, "audit_id", exact = TRUE))
  x$audit_id
}

evidence_is_bound <- function(calibration, evidence) {
  calibration_id <- calibration$audit_id
  evidence_id <- evidence_audit_id(evidence)
  !is.null(calibration_id) && !is.null(evidence_id) &&
    identical(calibration_id, evidence_id)
}

same_confidence_level <- function(actual, required) {
  length(actual) == 1L && is.finite(actual) &&
    isTRUE(all.equal(as.numeric(actual), as.numeric(required), tolerance = 1e-12))
}

confidence_level_check <- function(check, actual, required) {
  if (same_confidence_level(actual, required)) {
    assessment_row(
      check, "PASS", actual, paste0("= ", required), "Confidence level matches policy."
    )
  } else {
    assessment_row(
      check, "INDETERMINATE", actual, paste0("= ", required),
      "Evidence confidence level does not match the declared policy."
    )
  }
}

assessment_row <- function(check, status, value, threshold, reason) {
  tibble::tibble(
    check = check,
    status = status,
    value = as.numeric(value),
    threshold = as.character(threshold),
    reason = reason
  )
}

compare_min_count <- function(check, value, threshold) {
  if (value >= threshold) {
    assessment_row(check, "PASS", value, paste0(">= ", threshold), "Sample size is sufficient.")
  } else {
    assessment_row(
      check, "INDETERMINATE", value, paste0(">= ", threshold),
      "Sample size is below the declared minimum."
    )
  }
}

compare_max_value <- function(check, value, threshold, fail_reason) {
  if (is.finite(value) && value <= threshold) {
    assessment_row(check, "PASS", value, paste0("<= ", threshold), "Criterion met.")
  } else {
    assessment_row(check, "FAIL", value, paste0("<= ", threshold), fail_reason)
  }
}

interval_min_check <- function(check, low, high, threshold) {
  if (!is.finite(low) || !is.finite(high)) {
    return(assessment_row(
      check, "INDETERMINATE", NA_real_, paste0("lower >= ", threshold),
      "Interval is unavailable."
    ))
  }
  if (low >= threshold) {
    assessment_row(check, "PASS", low, paste0("lower >= ", threshold), "Interval clears threshold.")
  } else if (high < threshold) {
    assessment_row(check, "FAIL", low, paste0("lower >= ", threshold), "Interval is below threshold.")
  } else {
    assessment_row(
      check, "INDETERMINATE", low, paste0("lower >= ", threshold),
      "Interval overlaps threshold."
    )
  }
}

interval_max_check <- function(check, low, high, threshold) {
  if (!is.finite(low) || !is.finite(high)) {
    return(assessment_row(
      check, "INDETERMINATE", NA_real_, paste0("upper <= ", threshold),
      "Interval is unavailable."
    ))
  }
  if (high <= threshold) {
    assessment_row(check, "PASS", high, paste0("upper <= ", threshold), "Interval clears threshold.")
  } else if (low > threshold) {
    assessment_row(check, "FAIL", high, paste0("upper <= ", threshold), "Interval exceeds threshold.")
  } else {
    assessment_row(
      check, "INDETERMINATE", high, paste0("upper <= ", threshold),
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
    if (is.na(i)) {
      return(assessment_row(
        check, "NOT_ASSESSED", NA_real_, paste0("lower >= ", thresholds[[label]]),
        "Policy names a class absent from the calibration label universe."
      ))
    }
    low <- metrics[[paste0(metric, "_conf_low")]][[i]]
    high <- metrics[[paste0(metric, "_conf_high")]][[i]]
    if (!is.finite(low) || !is.finite(high)) {
      return(assessment_row(
        check, "INDETERMINATE", metrics[[metric]][[i]],
        paste0("lower >= ", thresholds[[label]]), "Class interval is undefined."
      ))
    }
    interval_min_check(check, low, high, thresholds[[label]])
  })
  do.call(vctrs::vec_rbind, rows)
}

overall_assessment <- function(status) {
  if (any(status == "FAIL")) return("FAIL")
  if (any(status == "NOT_ASSESSED")) return("NOT_ASSESSED")
  if (any(status == "INDETERMINATE")) return("INDETERMINATE")
  "PASS"
}
