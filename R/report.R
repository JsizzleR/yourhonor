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

# Report sections -------------------------------------------------------------
#
# A report is one list of sections, built once and rendered twice: the
# console and Markdown renderers differ only in how they draw a block and in
# the inline styles (`style`) the sections are written with. A section added
# here appears in both renderings; it cannot be written for one and forgotten
# in the other, which is how the two hand-written renderers drifted.
#
# A section returns a list of blocks:
#   title  the decision and the usable-item count; each renderer frames it
#          its own way (a console rule, a Markdown heading);
#   para   one sentence or paragraph;
#   list   a labelled list of items (a heading in Markdown);
#   note   a paragraph the console raises as an alert (`kind`).
report_sections <- function(report, style) {
  c(
    title_section(report),
    agreement_section(report, style),
    class_section(report, style),
    reliability_section(report, style),
    bias_section(report, style),
    stability_section(report, style),
    binding_section(report, style),
    checks_section(report, style)
  )
}

block_para <- function(text) list(list(type = "para", text = text))

# A list with nothing in it reads as not available in both renderings, never
# as an empty heading or bullet.
block_list <- function(label, items) {
  if (length(items) == 0L) {
    return(block_para(paste0(label, ": not available.")))
  }
  list(list(type = "list", label = label, items = items))
}

block_note <- function(text, kind = c("info", "warning")) {
  list(list(type = "note", text = text, kind = rlang::arg_match(kind)))
}

# Inline styles. The console ones go through cli's own theme, so the plain
# text matches the Markdown with its markup removed.
console_inline <- function(markup) {
  force(markup)
  function(x) {
    vapply(x, function(one) {
      cli::format_inline(paste0("{", markup, " {one}}"))
    }, character(1), USE.NAMES = FALSE)
  }
}

report_styles <- list(
  console = list(
    name = "console",
    strong = console_inline(".strong"),
    val = console_inline(".val"),
    code = console_inline(".code")
  ),
  md = list(
    name = "md",
    strong = function(x) paste0("**", x, "**"),
    val = function(x) paste0("`", x, "`"),
    code = function(x) paste0("`", x, "`")
  )
)

title_section <- function(report) {
  x <- report$calibration
  list(list(
    type = "title", decision = report$decision,
    n_items = x$n_items, n_input = x$n_input
  ))
}

# The headline labels the bootstrap interval and shows the exact lower bound
# beside it, because a policy assesses the smaller of the two: an interval
# printed bare read as the bound the check compared when it was not.
agreement_section <- function(report, style) {
  x <- report$calibration
  head <- headline_row(x, report$policy)
  if (!identical(head$status, "ok")) {
    return(block_para(sprintf(
      "Agreement with humans (%s): not estimable (%s).",
      head$coefficient, head$status
    )))
  }
  exact <- head$conf_low_exact
  exact_text <- if (length(exact) == 1L && is.finite(exact)) {
    paste0(", exact lower bound ", fmt_num(exact))
  } else {
    ""
  }
  interval <- if (isTRUE(head$boundary_degenerate)) {
    paste0(
      conf_level_label(x$conf_level),
      "bootstrap interval degenerate (zero width at the boundary)"
    )
  } else {
    sprintf(
      "%sbootstrap interval [%s, %s]",
      conf_level_label(x$conf_level),
      fmt_num(head$conf_low), fmt_num(head$conf_high)
    )
  }
  block_para(sprintf(
    "Agreement with humans: %s (%s); %s%s.",
    style$strong(paste0(head$coefficient, " = ", fmt_num(head$estimate))),
    head$interpretation, interval, exact_text
  ))
}

conf_level_label <- function(conf_level) {
  if (length(conf_level) != 1L || !is.finite(conf_level)) return("")
  # Ten significant digits: enough that no level short of 1 reads as 100%,
  # few enough that 0.95 does not print its binary residue.
  paste0(trimws(formatC(100 * conf_level, format = "fg", digits = 10)), "% ")
}

# Every class is listed, so a rare class with poor recall is never hidden
# behind a single summary line.
class_section <- function(report, style) {
  metrics <- report$calibration$confusion_metrics
  if (is.null(metrics)) return(block_list("Per-class evidence", character(0)))
  block_list("Per-class evidence", sprintf(
    "%s: support %d, precision %s, recall %s",
    style$val(metrics$label), as.integer(metrics$support),
    fmt_num(metrics$precision), fmt_num(metrics$recall)
  ))
}

reliability_section <- function(report, style) {
  reliability <- human_reliability_gwet(report$calibration)
  if (is.null(reliability)) return(list())
  block_para(sprintf(
    "Human reliability benchmark: %s = %s.",
    reliability$coefficient, fmt_num(reliability$estimate)
  ))
}

bias_section <- function(report, style) {
  if (is.null(report$bias)) {
    return(block_para(paste0(
      "Bias probes: ", style$strong("NOT ASSESSED"), "."
    )))
  }
  block_para(bias_summary(report$bias, report$checks, report$policy))
}

# The stability evidence is one section, so a line added to it (a further
# bound, say) is written here once.
stability_section <- function(report, style) {
  stability <- report$stability
  if (is.null(stability)) {
    return(block_para(paste0(
      "Test-retest stability: ", style$strong("NOT ASSESSED"), "."
    )))
  }
  block_para(sprintf(
    "Pairwise disagreement: %s [%s, %s].",
    fmt_num(stability$pairwise_disagreement),
    fmt_num(stability$pairwise_disagreement_conf_low),
    fmt_num(stability$pairwise_disagreement_conf_high)
  ))
}

binding_section <- function(report, style) {
  x <- report$calibration
  if (supplied_evidence_is_bound(x, report$bias, report$stability)) {
    return(list())
  }
  block_note(paste0(
    "Evidence binding: ", style$strong("UNBOUND"), ". Use one ",
    style$code("audit_id"), " across calibration, bias, and stability."
  ), kind = "warning")
}

# Without a policy the checks hold one NOT_ASSESSED row saying so; it is
# rendered like any other check rather than replaced by a separate notice.
checks_section <- function(report, style) {
  block_list("Assessment checks", check_items(report$checks, style$name))
}

render_console_report <- function(report) {
  for (block in report_sections(report, report_styles$console)) {
    switch(block$type,
      title = {
        cli::cli_h1("yourhonor assessment: {block$decision}")
        cli::cli_text(
          "Judge evidence: n = {block$n_items} of {block$n_input}."
        )
      },
      para = cli::cli_text("{block$text}"),
      list = {
        cli::cli_text("{block$label}:")
        cli::cli_ul()
        for (item in block$items) cli::cli_li("{item}")
        cli::cli_end()
      },
      note = if (identical(block$kind, "warning")) {
        cli::cli_alert_warning("{block$text}")
      } else {
        cli::cli_alert_info("{block$text}")
      }
    )
  }
  invisible(NULL)
}

build_markdown <- function(report) {
  blocks <- report_sections(report, report_styles$md)
  rendered <- lapply(blocks, function(block) {
    switch(block$type,
      title = c(
        sprintf(
          "## Judge evidence (n = %d of %d)", block$n_items, block$n_input
        ),
        "",
        sprintf("Assessment: **%s**.", block$decision)
      ),
      para = block$text,
      list = c(paste("###", block$label), "", paste0("- ", block$items)),
      note = block$text
    )
  })
  # Every block is its own Markdown paragraph.
  utils::head(unlist(lapply(rendered, function(lines) c(lines, ""))), -1L)
}

# Render `value (op threshold)` for every check so a FAIL line shows how far
# the evidence sat from its gate; the policy-less placeholder has neither.
format_check_lines <- function(checks, style = c("console", "md")) {
  style <- rlang::arg_match(style)
  items <- check_items(checks, style)
  if (style == "md") paste0("- ", items) else items
}

check_items <- function(checks, style = c("console", "md")) {
  style <- rlang::arg_match(style)
  pair <- fmt_check_pair(checks$value, checks$threshold)
  measure <- ifelse(
    is.na(checks$op),
    "",
    paste0(" = ", pair$value, " (", checks$op, " ", pair$threshold, ")")
  )
  if (style == "md") {
    sprintf(
      "**%s** `%s`%s: %s",
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

# The fields report() and write_audit() read from supplied evidence. They
# are named once here, and test-report-render.R checks that the outputs of
# probe_bias(), probe_position(), combine_bias() and test_retest() carry
# every one, so a constructor that drops a field fails a test rather than
# a caller's report.
report_bias_fields <- c(
  "probe", "flip_rate", "directional_rate", "directional_conf_low",
  "directional_conf_high", "conf_low", "conf_high",
  "n_scored", "n_input", "n_unscored", "statistic", "n_directional_items",
  "missing_rate", "n_discordant", "p_value", "verdict", "status",
  "conf_level"
)

report_stability_fields <- c(
  "pairwise_disagreement", "pairwise_disagreement_conf_low",
  "pairwise_disagreement_conf_high", "missing_rate", "pairwise_ci_reliable",
  "n_items", "n_items_scored", "n_runs", "label_levels", "weights",
  "conf_level", "n_boot", "pairwise_n_boot_effective",
  "stability_n_boot_effective", "stability", "stability_coefficient",
  "stability_conf_low", "stability_conf_high", "stability_status",
  "stability_boundary_degenerate", "stability_ci_reliable", "audit_id"
)

check_evidence_fields <- function(evidence, required, arg,
                                  call = rlang::caller_env()) {
  missing <- setdiff(required, names(evidence))
  if (length(missing) > 0L) {
    cli::cli_abort(
      paste0(
        "{.arg {arg}} lacks required audit ",
        "{cli::qty(missing)}field{?s} {.field {missing}}."
      ),
      call = call
    )
  }
}

validate_report_evidence <- function(x, bias, stability,
                                     call = rlang::caller_env()) {
  if (!is.null(bias)) {
    if (!inherits(bias, "yourhonor_bias")) {
      cli::cli_abort(
        "{.arg bias} must be a {.cls yourhonor_bias} object.", call = call
      )
    }
    check_evidence_fields(bias, report_bias_fields, "bias", call = call)
  }
  if (!is.null(stability)) {
    if (!inherits(stability, "yourhonor_stability")) {
      cli::cli_abort(
        "{.arg stability} must be a {.cls yourhonor_stability} object.",
        call = call
      )
    }
    check_evidence_fields(
      stability, report_stability_fields, "stability", call = call
    )
  }

  ids <- lapply(Filter(Negate(is.null), list(x, bias, stability)), audit_id)
  present <- unlist(Filter(is_binding_id, ids), use.names = FALSE)
  if (length(unique(present)) > 1L) {
    cli::cli_abort(
      "Calibration, bias, and stability {.arg audit_id} values do not match.",
      call = call
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

#' @export
print.yourhonor_policy <- function(x, ...) {
  cli::cli_h1("yourhonor acceptance policy")
  coefficient <- if (is.null(x$coefficient)) {
    paste(
      "the calibration's Gwet coefficient",
      "(gwet_ac1 unweighted, gwet_ac2 weighted)"
    )
  } else {
    x$coefficient
  }
  cli::cli_text("Agreement coefficient: {coefficient}.")
  cli::cli_text("Confidence level: {fmt_check_number(x$conf_level)}.")

  policy_gate_list("Calibration", c(
    policy_gate("agreement", "lower >=", x$min_agreement_lower),
    policy_gate("usable items", ">=", x$min_n),
    policy_gate("class support", ">=", x$min_class_n),
    policy_gate("recall", "lower >=", x$min_recall),
    policy_gate("precision", "lower >=", x$min_precision),
    policy_gate("missing rate", "<=", x$max_missing_rate),
    policy_gate("judge abstention rate", "upper <=", x$max_abstention_rate),
    policy_gate("human reliability", "lower >=", x$min_human_reliability_lower)
  ))

  if (is.null(x$max_pairwise_disagreement)) {
    cli::cli_text("Stability: outside the policy.")
  } else {
    policy_gate_list("Stability", c(
      policy_gate(
        "pairwise disagreement", "upper <=", x$max_pairwise_disagreement
      ),
      policy_gate("items", ">=", x$min_stability_n),
      policy_gate("runs", ">=", x$min_stability_runs),
      policy_gate("missing rate", "<=", x$max_stability_missing_rate)
    ))
  }

  if (length(x$required_probes) == 0L) {
    cli::cli_text("Bias: outside the policy.")
  } else {
    probes <- x$required_probes
    policy_gate_list("Bias (required probes: {.val {probes}})", c(
      policy_gate("scored items", ">=", x$min_probe_n),
      policy_gate("flip rate", "upper <=", x$max_probe_flip_rate),
      policy_gate("directional rate", "upper <=", x$max_probe_directional_rate),
      policy_gate("missing rate", "<=", x$max_probe_missing_rate)
    ))
  }
  cli::cli_text(
    "Assess it with {.code report(calibration, policy = <this policy>)}."
  )
  invisible(x)
}

# One `name op threshold` line per gate the policy sets; a gate left NULL is
# not listed. A named threshold (per-class recall or precision) lists each
# class.
policy_gate <- function(name, op, threshold) {
  if (is.null(threshold)) return(character(0))
  values <- vapply(threshold, fmt_check_number, character(1))
  if (is.null(names(threshold))) {
    return(paste(name, op, values))
  }
  paste0(name, " (", names(threshold), ") ", op, " ", values)
}

policy_gate_list <- function(label, gates, .envir = parent.frame()) {
  cli::cli_text(paste0(label, ":"), .envir = .envir)
  cli::cli_ul()
  for (gate in gates) cli::cli_li("{gate}")
  cli::cli_end()
}
