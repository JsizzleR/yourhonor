#' Report judge evidence or assess an explicit policy
#'
#' Without `policy`, this function renders descriptive evidence and returns the
#' decision `"NOT_ASSESSED"`. It never infers that missing bias or stability
#' evidence passed. With an [acceptance_policy()], every criterion is rendered as
#' `PASS`, `FAIL`, `NOT_ASSESSED`, or `INDETERMINATE`.
#'
#' The result is the same object whichever `output` is chosen; `output` only
#' sets how it prints by default. `print(x, output = "md")` or
#' `format(x, output = "md")` render the Markdown form of any report, and
#' `as.character()` always returns the Markdown lines.
#'
#' @param x A `yourhonor_calibration` object.
#' @param bias Optional result from [probe_bias()], [probe_position()], or
#'   [combine_bias()].
#' @param stability Optional result from [test_retest()].
#' @param policy Optional policy from [acceptance_policy()].
#' @param output Default rendering when the report is printed: `"console"`
#'   (styled cli output) or `"md"` (Markdown lines).
#'
#' @return A `yourhonor_report` list with elements `decision` (one of `PASS`,
#'   `FAIL`, `INDETERMINATE`, `NOT_ASSESSED`), `checks` (a tibble with one row
#'   per policy check: `check`, `status`, `value`, `op`, `threshold`, and
#'   `reason`), the supplied `calibration`, `bias`, `stability`, and `policy`,
#'   and the default `output`. The report is returned visibly, so calling
#'   `report()` at the console renders it.
#' @export
#' @examples
#' gold <- tibble::tibble(
#'   human_label = c("C", "C", "P", "I", "C", "P", "I", "C"),
#'   judge_label = c("C", "C", "C", "I", "C", "P", "P", "C")
#' )
#' cal <- calibrate_judge(gold, label_levels = c("I", "P", "C"),
#'                        n_boot = 200, seed = 1)
#' rep <- report(cal, output = "md")
#' rep$decision
#' rep
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
  structure(
    list(
      decision = assessment$decision,
      checks = assessment$checks,
      calibration = x,
      bias = bias,
      stability = stability,
      policy = policy,
      output = output
    ),
    class = "yourhonor_report"
  )
}

report_outputs <- c("console", "md")

#' @export
print.yourhonor_report <- function(x, output = x$output, ...) {
  output <- rlang::arg_match(output, report_outputs)
  if (output == "md") {
    cat(format(x, output = "md"), sep = "\n")
  } else {
    render_console_report(x)
  }
  invisible(x)
}

#' @export
format.yourhonor_report <- function(x, output = x$output, ...) {
  output <- rlang::arg_match(output, report_outputs)
  if (output == "md") {
    build_markdown(x)
  } else {
    cli::cli_fmt(render_console_report(x))
  }
}

#' @export
as.character.yourhonor_report <- function(x, ...) {
  format(x, output = "md")
}

render_console_report <- function(report) {
  x <- report$calibration
  bias <- report$bias
  stability <- report$stability
  policy <- report$policy
  head <- headline_row(x, policy)

  cli::cli_h1("yourhonor assessment: {report$decision}")
  cli::cli_text("Judge evidence: n = {x$n_items} of {x$n_input}.")
  print_agreement_line(head)
  print_class_lines(x$confusion_metrics)
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
    cli::cli_text("{bias_summary(bias, report$checks, policy)}")
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
    lines <- format_check_lines(report$checks, style = "console")
    for (line in lines) cli::cli_text("{line}")
  }
  if (!supplied_evidence_is_bound(x, bias, stability)) {
    cli::cli_alert_warning(
      "Supplied secondary evidence is UNBOUND; use one audit_id across all runs."
    )
  }
  invisible(NULL)
}

# Render `value (op threshold)` for every check so a FAIL line shows how far
# the evidence sat from its gate; the policy-less placeholder has neither.
format_check_lines <- function(checks, style = c("console", "md")) {
  style <- rlang::arg_match(style)
  pair <- fmt_check_pair(checks$value, checks$threshold)
  measure <- ifelse(
    is.na(checks$op),
    "",
    paste0(" = ", pair$value, " (", checks$op, " ", pair$threshold, ")")
  )
  if (style == "md") {
    sprintf(
      "- **%s** `%s`%s: %s",
      checks$status, checks$check, measure, checks$reason
    )
  } else {
    sprintf(
      "%s: %s%s - %s",
      checks$status, checks$check, measure, checks$reason
    )
  }
}

# Counts print as integers and rates keep three significant digits, so a
# missing rate of 0.003 is not rendered as the "0.00" of a two-decimal format.
# A value and threshold that differ must also render differently, otherwise
# a FAIL at 0.1005 against 0.1 would read as "0.1 (<= 0.1)"; the precision is
# raised for that pair until the two strings part. Past 15 significant digits
# `format()` can still merge neighbouring doubles, so the pair then falls back
# to the 17 digits that identify every double uniquely.
fmt_check_pair <- function(value, threshold) {
  rendered <- vapply(seq_along(value), function(i) {
    v <- value[[i]]
    t <- threshold[[i]]
    if (is.na(v) || is.na(t) || v == t) {
      return(c(fmt_check_number(v), fmt_check_number(t)))
    }
    for (digits in 3:15) {
      out <- c(fmt_check_number(v, digits), fmt_check_number(t, digits))
      if (out[[1]] != out[[2]]) return(out)
    }
    c(fmt_check_number(v, exact = TRUE), fmt_check_number(t, exact = TRUE))
  }, character(2))
  list(value = rendered[1, ], threshold = rendered[2, ])
}

fmt_check_number <- function(x, digits = 3L, exact = FALSE) {
  if (is.na(x)) return("NA")
  if (exact) sprintf("%.17g", x) else format(x, digits = digits)
}

headline_row <- function(x, policy = NULL) {
  ag <- x$agreement
  coefficient <- if (is.null(policy)) {
    preferred <- c("gwet_ac2", "gwet_ac1")
    match <- preferred[preferred %in% ag$coefficient]
    if (length(match) > 0L) match[[1]] else ag$coefficient[[1]]
  } else {
    policy_coefficient(policy, x)
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

# Policy checks exist only for `required_probes`, so a detected signal in any
# other supplied probe is reported alongside the gate outcome rather than
# hidden behind it.
bias_summary <- function(bias, checks, policy) {
  if (is.null(policy)) {
    biases <- detected_biases(bias)
    if (length(biases) > 0L) {
      return(paste0(
        "Descriptive probe settings detected bias: ",
        paste(biases, collapse = ", "), "."
      ))
    }
    if (any(bias$verdict == "INCONCLUSIVE")) {
      return("At least one supplied bias probe was INCONCLUSIVE.")
    }
    return(paste0(
      "Supplied bias probes found no directional signal; ",
      "absence is not established."
    ))
  }

  required <- policy$required_probes
  outside <- detected_biases(bias[!bias$probe %in% required, , drop = FALSE])
  # An unscored probe outside the policy is not a gate outcome, but hiding
  # it would let the summary imply the extra probe produced evidence.
  unscored <- bias$probe[
    !bias$probe %in% required & !is.na(bias$verdict) &
      bias$verdict == "INCONCLUSIVE"
  ]
  if (length(required) == 0L) {
    state <- if (length(outside) > 0L) {
      paste0(
        "descriptive probe settings flagged ", paste(outside, collapse = ", ")
      )
    } else {
      "descriptive probe settings did not flag a directional signal"
    }
    return(append_unscored(
      paste0("Bias probes were outside the policy; ", state, "."), unscored
    ))
  }
  bias_checks <- checks[startsWith(checks$check, "bias_"), , drop = FALSE]
  failed <- bias_checks$check[bias_checks$status == "FAIL"]
  incomplete <- bias_checks$status %in% c("NOT_ASSESSED", "INDETERMINATE")
  summary <- if (length(failed) > 0L) {
    paste0("Policy-failing bias checks: ", paste(failed, collapse = ", "), ".")
  } else if (any(incomplete)) {
    "Bias policy evidence is incomplete or indeterminate."
  } else {
    "Required bias probes cleared the declared policy gates."
  }
  if (length(outside) > 0L) {
    summary <- paste0(
      summary, " Outside the policy, descriptive probe settings flagged ",
      paste(outside, collapse = ", "), "."
    )
  }
  append_unscored(summary, unscored)
}

append_unscored <- function(summary, unscored) {
  if (length(unscored) == 0L) return(summary)
  verb <- if (length(unscored) == 1L) "was" else "were"
  paste0(
    summary, " Outside the policy, ", paste(unscored, collapse = ", "),
    " ", verb, " INCONCLUSIVE."
  )
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

  ids <- lapply(Filter(Negate(is.null), list(x, bias, stability)), audit_id)
  present <- unlist(Filter(is_binding_id, ids), use.names = FALSE)
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

# The console shows the same per-class facts as the Markdown report, so a
# rare class with poor recall is never hidden behind a single summary line.
print_class_lines <- function(metrics) {
  if (is.null(metrics) || nrow(metrics) == 0L) {
    cli::cli_text("Per-class evidence: not available.")
    return(invisible(NULL))
  }
  cli::cli_text("Per-class evidence:")
  cli::cli_ul()
  for (i in seq_len(nrow(metrics))) {
    row <- metrics[i, ]
    cli::cli_li(paste0(
      "{.val {row$label}}: support {row$support}, precision ",
      "{fmt_num(row$precision)}, recall {fmt_num(row$recall)}"
    ))
  }
  cli::cli_end()
  invisible(NULL)
}

build_markdown <- function(report) {
  x <- report$calibration
  bias <- report$bias
  stability <- report$stability
  policy <- report$policy
  head <- headline_row(x, policy)
  lines <- c(
    sprintf("## Judge evidence (n = %d of %d)", x$n_items, x$n_input),
    "",
    sprintf("Assessment: **%s**.", report$decision),
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
    lines <- c(lines, "", bias_summary(bias, report$checks, policy))
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

  c(
    lines, "", "### Assessment checks", "",
    format_check_lines(report$checks, style = "md")
  )
}
