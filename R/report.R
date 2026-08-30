#' Report judge evidence or assess an explicit policy
#'
#' Without `policy`, this function renders descriptive evidence and returns the
#' decision `"NOT_ASSESSED"`. It never infers that missing bias or stability
#' evidence passed. With an [acceptance_policy()], every criterion is rendered as
#' `PASS`, `FAIL`, `NOT_ASSESSED`, or `INDETERMINATE`.
#'
#' @param x A `yourhonor_calibration` object.
#' @param bias Optional result from [probe_bias()] or [probe_position()].
#' @param stability Optional result from [test_retest()].
#' @param policy Optional policy from [acceptance_policy()].
#' @param output `"console"` or `"md"`.
#'
#' @return For Markdown, a `yourhonor_report` character vector with `decision`
#'   and `checks` attributes. Console output returns `x` invisibly.
#' @export
#' @examples
#' gold <- tibble::tibble(
#'   human_label = c("C", "C", "P", "I", "C", "P", "I", "C"),
#'   judge_label = c("C", "C", "C", "I", "C", "P", "P", "C")
#' )
#' cal <- calibrate_judge(gold, label_levels = c("I", "P", "C"),
#'                        n_boot = 200, seed = 1)
#' report(cal, output = "md")
report <- function(x,
                   bias = NULL,
                   stability = NULL,
                   policy = NULL,
                   output = c("console", "md")) {
  if (!inherits(x, "yourhonor_calibration")) {
    cli::cli_abort("{.arg x} must be a {.cls yourhonor_calibration} object.")
  }
  validate_report_evidence(x, bias, stability)
  output <- rlang::arg_match(output)
  assessment <- assess_calibration(x, bias, stability, policy)
  head <- headline_row(x, policy)
  bundle_bound <- supplied_evidence_is_bound(x, bias, stability)

  if (output == "md") {
    md <- build_markdown(x, head, bias, stability, assessment)
    return(invisible(structure(
      md,
      class = "yourhonor_report",
      decision = assessment$decision,
      checks = assessment$checks
    )))
  }

  cli::cli_h1("yourhonor assessment: {assessment$decision}")
  print_agreement_line(head)
  print_class_line(x$confusion_metrics)
  reliability <- human_reliability_gwet(x)
  if (!is.null(reliability)) {
    cli::cli_text(
      "Human reliability benchmark: {reliability$coefficient} = ",
      "{fmt_num(reliability$estimate)}."
    )
  }
  if (is.null(bias)) {
    cli::cli_text("Bias probes: NOT ASSESSED.")
  } else {
    cli::cli_text("{bias_summary(bias, assessment)}")
  }
  if (is.null(stability)) {
    cli::cli_text("Test-retest stability: NOT ASSESSED.")
  } else {
    cli::cli_text(
      "Pairwise disagreement: {fmt_num(stability$pairwise_disagreement)} ",
      "[{fmt_num(stability$pairwise_disagreement_conf_low)}, ",
      "{fmt_num(stability$pairwise_disagreement_conf_high)}]."
    )
  }
  if (is.null(policy)) {
    cli::cli_alert_info("Descriptive only: no acceptance policy was supplied.")
  } else {
    cli::cli_h2("policy checks")
    for (i in seq_len(nrow(assessment$checks))) {
      row <- assessment$checks[i, ]
      cli::cli_text("{row$status}: {row$check} - {row$reason}")
    }
  }
  if (!bundle_bound) {
    cli::cli_alert_warning(
      "Supplied secondary evidence is UNBOUND; use one audit_id across all runs."
    )
  }
  invisible(x)
}

#' @export
print.yourhonor_report <- function(x, ...) {
  cat(unclass(x), sep = "\n")
  invisible(x)
}

headline_row <- function(x, policy = NULL) {
  ag <- x$agreement
  coefficient <- if (is.null(policy)) {
    preferred <- c("gwet_ac2", "gwet_ac1")
    match <- preferred[preferred %in% ag$coefficient]
    if (length(match) > 0L) match[[1]] else ag$coefficient[[1]]
  } else {
    policy$coefficient
  }
  i <- which(ag$coefficient == coefficient)
  if (length(i) == 0L) {
    return(list(
      coefficient = coefficient,
      estimate = NA_real_, conf_low = NA_real_, conf_high = NA_real_,
      status = "not_computed", interpretation = NA_character_
    ))
  }
  as.list(ag[i[[1]], ])
}

detected_biases <- function(bias) {
  if (is.null(bias)) return(character(0))
  bias$probe[!is.na(bias$verdict) & bias$verdict == "BIAS"]
}

bias_summary <- function(bias, assessment) {
  checks <- assessment$checks
  policy_supplied <- !any(checks$check == "acceptance_policy")
  if (policy_supplied) {
    bias_checks <- checks[startsWith(checks$check, "bias_"), , drop = FALSE]
    if (nrow(bias_checks) == 0L) {
      biases <- detected_biases(bias)
      state <- if (length(biases) > 0L) {
        paste0("descriptive probe settings flagged ", paste(biases, collapse = ", "))
      } else {
        "descriptive probe settings did not flag a directional signal"
      }
      return(paste0("Bias probes were outside the policy; ", state, "."))
    }
    failed <- bias_checks$check[bias_checks$status == "FAIL"]
    incomplete <- bias_checks$status %in% c("NOT_ASSESSED", "INDETERMINATE")
    if (length(failed) > 0L) {
      return(paste0("Policy-failing bias checks: ", paste(failed, collapse = ", "), "."))
    }
    if (any(incomplete)) {
      return("Bias policy evidence is incomplete or indeterminate.")
    }
    return("Supplied bias probes cleared the declared policy gates.")
  }
  biases <- detected_biases(bias)
  if (length(biases) > 0L) {
    paste0(
      "Descriptive probe settings detected bias: ", paste(biases, collapse = ", "), "."
    )
  } else if (any(bias$verdict == "INCONCLUSIVE")) {
    "At least one supplied bias probe was INCONCLUSIVE."
  } else {
    "Supplied bias probes found no directional signal; absence is not established."
  }
}

validate_report_evidence <- function(x, bias, stability) {
  if (!is.null(bias)) {
    if (!inherits(bias, "yourhonor_bias")) {
      cli::cli_abort("{.arg bias} must be a {.cls yourhonor_bias} object.")
    }
    required <- c(
      "probe", "flip_rate", "directional_rate", "directional_conf_low",
      "directional_conf_high", "conf_low", "conf_high",
      "n_scored", "n_input",
      "missing_rate", "n_discordant", "p_value", "verdict", "status",
      "conf_level"
    )
    if (!all(required %in% names(bias))) {
      cli::cli_abort("{.arg bias} lacks required audit fields.")
    }
  }
  if (!is.null(stability)) {
    if (!inherits(stability, "yourhonor_stability")) {
      cli::cli_abort("{.arg stability} must be a {.cls yourhonor_stability} object.")
    }
    required <- c(
      "pairwise_disagreement", "pairwise_disagreement_conf_low",
      "pairwise_disagreement_conf_high", "missing_rate", "pairwise_ci_reliable",
      "n_items", "n_items_scored", "n_runs", "label_levels", "weights",
      "conf_level", "n_boot", "pairwise_n_boot_effective",
      "stability_n_boot_effective", "stability", "stability_coefficient",
      "stability_conf_low", "stability_conf_high", "stability_status",
      "stability_boundary_degenerate", "stability_ci_reliable", "audit_id"
    )
    if (!all(required %in% names(stability))) {
      cli::cli_abort("{.arg stability} lacks required audit fields.")
    }
  }

  ids <- list(
    calibration = x$audit_id,
    bias = if (is.null(bias)) NULL else evidence_audit_id(bias),
    stability = if (is.null(stability)) NULL else evidence_audit_id(stability)
  )
  present <- unlist(ids[!vapply(ids, is.null, logical(1))], use.names = FALSE)
  if (length(unique(present)) > 1L) {
    cli::cli_abort(
      "Calibration, bias, and stability {.arg audit_id} values do not match."
    )
  }
  invisible(NULL)
}

supplied_evidence_is_bound <- function(x, bias, stability) {
  supplied <- Filter(Negate(is.null), list(bias, stability))
  if (length(supplied) == 0L) return(TRUE)
  all(vapply(supplied, function(evidence) {
    evidence_is_bound(x, evidence)
  }, logical(1)))
}

human_reliability_gwet <- function(x) {
  hr <- x$human_reliability
  if (is.null(hr)) return(NULL)
  row <- hr[hr$coefficient %in% c("gwet_ac2", "gwet_ac1"), , drop = FALSE]
  if (nrow(row) == 0L) return(NULL)
  list(coefficient = row$coefficient[[1]], estimate = row$estimate[[1]])
}

print_agreement_line <- function(head) {
  if (!identical(head$status, "ok")) {
    cli::cli_text(
      "Agreement evidence ({head$coefficient}): not estimable ({head$status})."
    )
  } else {
    cli::cli_text(
      "Agreement evidence: {.strong {head$coefficient}} = {fmt_num(head$estimate)} ",
      "[{fmt_num(head$conf_low)}, {fmt_num(head$conf_high)}] ({head$interpretation})."
    )
  }
}

print_class_line <- function(metrics) {
  valid <- is.finite(metrics$recall)
  if (!any(valid)) {
    cli::cli_text("Per-class recall: not estimable.")
    return(invisible(NULL))
  }
  worst <- metrics[which.min(replace(metrics$recall, !valid, Inf)), ]
  cli::cli_text(
    "Worst class recall: {.val {worst$label}} = {fmt_num(worst$recall)} ",
    "(support {worst$support})."
  )
  invisible(NULL)
}

build_markdown <- function(x, head, bias, stability, assessment) {
  lines <- c(
    sprintf("## Judge evidence (n = %d of %d)", x$n_items, x$n_input),
    "",
    sprintf("Assessment: **%s**.", assessment$decision),
    ""
  )
  if (identical(head$status, "ok")) {
    lines <- c(lines, sprintf(
      "Agreement with humans: **%s = %s** [%s, %s] (%s).",
      head$coefficient, fmt_num(head$estimate), fmt_num(head$conf_low),
      fmt_num(head$conf_high), head$interpretation
    ))
  } else {
    lines <- c(lines, sprintf(
      "Agreement with humans (%s): not estimable (%s).",
      head$coefficient, head$status
    ))
  }
  metrics <- x$confusion_metrics
  class_lines <- sprintf(
    "- `%s`: support %d, precision %s, recall %s",
    metrics$label, metrics$support, fmt_num(metrics$precision), fmt_num(metrics$recall)
  )
  lines <- c(lines, "", "### Per-class evidence", "", class_lines)

  reliability <- human_reliability_gwet(x)
  if (!is.null(reliability)) {
    lines <- c(lines, "", sprintf(
      "Human reliability benchmark: %s = %s.",
      reliability$coefficient, fmt_num(reliability$estimate)
    ))
  }
  if (is.null(bias)) {
    lines <- c(lines, "", "Bias probes: **NOT ASSESSED**.")
  } else {
    lines <- c(lines, "", bias_summary(bias, assessment))
  }
  if (is.null(stability)) {
    lines <- c(lines, "Test-retest stability: **NOT ASSESSED**.")
  } else {
    lines <- c(lines, sprintf(
      "Pairwise disagreement: %s [%s, %s].",
      fmt_num(stability$pairwise_disagreement),
      fmt_num(stability$pairwise_disagreement_conf_low),
      fmt_num(stability$pairwise_disagreement_conf_high)
    ))
  }

  if (!supplied_evidence_is_bound(x, bias, stability)) {
    lines <- c(
      lines, "",
      "Evidence binding: **UNBOUND**. Use one `audit_id` across calibration, bias, and stability."
    )
  }

  lines <- c(lines, "", "### Assessment checks", "")
  checks <- assessment$checks
  lines <- c(lines, sprintf(
    "- **%s** `%s`: %s",
    checks$status, checks$check, checks$reason
  ))
  lines
}
