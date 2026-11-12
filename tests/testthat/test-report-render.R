# Rendering of reports and policies: the section list both renderers share,
# the agreement headline's bounds, and the policy print method.

render_cal <- function(audit_id = "render-bundle") {
  human <- rep(c("I", "P", "C"), each = 100)
  judge <- human
  judge[c(1, 101, 201)] <- c("P", "C", "P")
  calibrate_judge(
    tibble::tibble(human_label = human, judge_label = judge),
    label_levels = c("I", "P", "C"), coefficients = "gwet_ac1",
    n_boot = 100, seed = 1, audit_id = audit_id
  )
}

render_policy <- function(...) {
  args <- utils::modifyList(
    list(
      min_agreement_lower = 0.9, min_n = 50, min_class_n = 15,
      min_recall = 0.8, max_missing_rate = 0,
      max_pairwise_disagreement = NULL, coefficient = "gwet_ac1"
    ),
    list(...)
  )
  do.call(acceptance_policy, args)
}

render_bias <- function(audit_id = "render-bundle") {
  probe_bias(
    tibble::tibble(input = paste("answer", seq_len(60))),
    function(d) rep("C", nrow(d)),
    probes = "verbosity", seed = 1, audit_id = audit_id
  )
}

render_stability <- function(audit_id = "render-bundle") {
  test_retest(
    tibble::tibble(input = paste("answer", seq_len(60))),
    function(d) rep("C", nrow(d)),
    input_cols = "input", n_runs = 3, label_levels = c("I", "P", "C"),
    n_boot = 50, seed = 1, audit_id = audit_id
  )
}

# Console text with styling, wrapping and bullets folded away, so a sentence
# can be looked up in it whatever the console width.
plain_console <- function(rep) {
  out <- cli::ansi_strip(format(rep, output = "console"))
  out <- gsub("[•*]", " ", out)
  gsub("\\s+", " ", paste(out, collapse = " "))
}

test_that("the markdown report separates the bias and stability paragraphs", {
  rep <- report(
    render_cal(), bias = render_bias(), stability = render_stability()
  )
  md <- format(rep, output = "md")
  stab <- grep("^Pairwise disagreement:", md)
  expect_length(stab, 1L)
  expect_identical(md[[stab - 1L]], "")
  expect_match(md[[stab - 2L]], "bias probes found no directional signal")

  # Neither supplied: the two NOT ASSESSED lines are separate paragraphs too.
  md <- format(report(render_cal()), output = "md")
  stab <- grep("^Test-retest stability:", md)
  expect_identical(md[[stab - 1L]], "")
  expect_match(md[[stab - 2L]], "^Bias probes:")
})

test_that("the agreement headline labels its interval and the assessed bound", {
  cal <- render_cal()
  row <- cal$agreement[cal$agreement$coefficient == "gwet_ac1", ]
  # The case the headline used to hide: the exact bound is the lower one, so
  # it is the bound the policy assesses.
  expect_lt(row$conf_low_exact, row$conf_low)
  rep <- report(cal, policy = render_policy())
  check <- rep$checks[rep$checks$check == "agreement_gwet_ac1", ]
  expect_equal(check$value, row$conf_low_exact)

  interval <- sprintf(
    "95%% bootstrap interval [%s, %s]",
    fmt_num(row$conf_low), fmt_num(row$conf_high)
  )
  exact <- sprintf("exact lower bound %s", fmt_num(row$conf_low_exact))
  md <- paste(format(rep, output = "md"), collapse = "\n")
  console <- plain_console(rep)
  for (out in list(md, console)) {
    expect_match(out, interval, fixed = TRUE)
    expect_match(out, exact, fixed = TRUE)
  }
  expect_match(md, "Agreement with humans: **gwet_ac1 = ", fixed = TRUE)

  # The interval is labelled at the calibration's own confidence level.
  cal90 <- calibrate_judge(
    tibble::tibble(
      human_label = rep(c("I", "C"), each = 50),
      judge_label = rep(c("I", "C"), each = 50)
    ),
    label_levels = c("I", "C"), coefficients = "gwet_ac1",
    conf_level = 0.9, n_boot = 50, seed = 1
  )
  md90 <- paste(format(report(cal90), output = "md"), collapse = "\n")
  expect_match(md90, "90% bootstrap interval degenerate", fixed = TRUE)
  expect_false(grepl("[1.00, 1.00]", md90, fixed = TRUE))
  expect_match(
    md90,
    sprintf("exact lower bound %s", fmt_num(cal90$agreement$conf_low_exact)),
    fixed = TRUE
  )
  cal90$agreement$boundary_degenerate <- FALSE
  md90 <- paste(format(report(cal90), output = "md"), collapse = "\n")
  expect_match(md90, "90% bootstrap interval [", fixed = TRUE)
})

test_that("both renderers write every section of every report", {
  cal <- render_cal()
  no_metrics <- cal
  no_metrics$confusion_metrics <- NULL
  reports <- list(
    descriptive = report(cal),
    full = report(
      cal, bias = render_bias(), stability = render_stability(),
      policy = render_policy(
        max_pairwise_disagreement = 0.1, min_stability_n = 50,
        min_stability_runs = 3, max_stability_missing_rate = 0,
        required_probes = "verbosity", min_probe_n = 20,
        max_probe_flip_rate = 0.1, max_probe_directional_rate = 0.1,
        max_probe_missing_rate = 0
      )
    ),
    unbound = report(cal, bias = render_bias(audit_id = NULL)),
    no_metrics = report(no_metrics, policy = render_policy())
  )
  # Markup aside, the two renderings are built from the same blocks; only
  # the check lines keep a format of their own per renderer.
  unmark <- function(x) gsub("[`\"*]", "", cli::ansi_strip(x))
  for (name in names(reports)) {
    rep <- reports[[name]]
    md <- report_sections(rep, report_styles$md)
    console <- report_sections(rep, report_styles$console)
    expect_identical(
      vapply(console, `[[`, "", "type"), vapply(md, `[[`, "", "type"),
      info = name
    )
    for (i in seq_along(md)) {
      if (identical(md[[i]]$type, "title")) next
      expect_identical(console[[i]]$label, md[[i]]$label, info = name)
      expect_identical(
        length(console[[i]]$items), length(md[[i]]$items), info = name
      )
      expect_identical(
        unmark(console[[i]]$text), unmark(md[[i]]$text), info = name
      )
      if (!identical(md[[i]]$label, "Assessment checks")) {
        expect_identical(
          unmark(console[[i]]$items), unmark(md[[i]]$items), info = name
        )
      }
    }
    # Every block reaches its rendering.
    md_out <- format(rep, output = "md")
    console_out <- plain_console(rep)
    for (block in md[-1]) {
      expect_all(
        c(
          block$text, if (length(block$items)) paste0("- ", block$items)
        ) %in% md_out,
        info = name
      )
    }
    for (block in console[-1]) {
      for (t in cli::ansi_strip(c(block$text, block$label, block$items))) {
        expect(
          grepl(gsub("\\s+", " ", t), console_out, fixed = TRUE),
          sprintf("%s: console text not rendered: %s", name, t)
        )
      }
    }
  }
})

test_that("the markdown report is one level-2 heading over separated blocks", {
  reports <- list(
    descriptive = report(render_cal()),
    full = report(
      render_cal(), bias = render_bias(), stability = render_stability(),
      policy = render_policy(
        max_pairwise_disagreement = 0.1, min_stability_n = 50,
        min_stability_runs = 3, max_stability_missing_rate = 0
      )
    ),
    unbound = report(render_cal(), bias = render_bias(audit_id = NULL))
  )
  for (name in names(reports)) {
    md <- format(reports[[name]], output = "md")
    n <- length(md)
    expect_match(md[[1]], "^## Judge evidence \\(n = 300 of 300\\)$", info = name)
    expect_identical(md[[2]], "", info = name)
    expect_match(md[[3]], "^Assessment: \\*\\*[A-Z_]+\\*\\*\\.$", info = name)
    # One title, no other level-1 or level-2 heading.
    expect_identical(sum(grepl("^#{1,2} ", md)), 1L, info = name)
    # Blocks are separated by exactly one blank line, with none at the ends.
    expect_false(identical(md[[n]], ""), info = name)
    expect_none(md[-1] == "" & md[-n] == "", info = name)
    blocks <- split(md, cumsum(md == ""))
    blocks <- lapply(unname(blocks), function(b) b[b != ""])
    is_heading <- vapply(blocks, function(b) grepl("^### ", b[[1]]), NA)
    is_bullets <- vapply(blocks, function(b) all(grepl("^- ", b)), NA)
    # Every block is one paragraph line, a list heading alone, or a run of
    # bullets; a heading is followed by its bullets and bullets follow one.
    expect_all(is_heading | is_bullets | lengths(blocks) == 1L, info = name)
    expect_all(lengths(blocks[is_heading]) == 1L, info = name)
    expect_all(is_bullets[which(is_heading) + 1L], info = name)
    expect_all(is_heading[which(is_bullets) - 1L], info = name)
  }
})

test_that("a report without class metrics says so in both renderings", {
  cal <- render_cal()
  cal$confusion_metrics <- NULL
  rep <- report(cal)
  md <- format(rep, output = "md")
  expect_false("### Per-class evidence" %in% md)
  expect_true("Per-class evidence: not available." %in% md)
  expect_match(
    plain_console(rep), "Per-class evidence: not available.", fixed = TRUE
  )
})

test_that("the unbound warning reads the same in both renderings", {
  rep <- report(render_cal(), bias = render_bias(audit_id = NULL))
  expect_true(
    paste0(
      "Evidence binding: **UNBOUND**. Use one `audit_id` across ",
      "calibration, bias, and stability."
    ) %in% format(rep, output = "md")
  )
  expect_match(
    plain_console(rep),
    paste(
      "Evidence binding: UNBOUND. Use one `audit_id` across calibration,",
      "bias, and stability."
    ),
    fixed = TRUE
  )
})

test_that("the stability headline labels its interval and the assessed bound", {
  # A judge that abstains on a third of its judgments: the worst-case bound
  # the policy assesses sits well above the observed interval.
  run <- 0
  judge <- function(d) {
    run <<- run + 1
    item <- as.integer(sub("answer ", "", d$input, fixed = TRUE))
    out <- rep("C", nrow(d))
    out[item %% 3L == run %% 3L] <- NA
    out
  }
  stability <- test_retest(
    tibble::tibble(input = paste("answer", seq_len(60))), judge,
    input_cols = "input", n_runs = 3, label_levels = c("I", "P", "C"),
    n_boot = 50, seed = 1, audit_id = "render-bundle"
  )
  worst <- stability$pairwise_disagreement_worst_case_high
  expect_gt(worst, stability$pairwise_disagreement_conf_high)
  rep <- report(
    render_cal(), stability = stability,
    policy = render_policy(
      max_pairwise_disagreement = 0.5, min_stability_n = 10,
      min_stability_runs = 2, max_stability_missing_rate = 1
    )
  )
  check <- rep$checks[rep$checks$check == "stability_pairwise_disagreement", ]
  expect_equal(check$value, worst)

  best <- stability$pairwise_disagreement_best_case_low
  expect_lte(best, stability$pairwise_disagreement_conf_low)
  line <- sprintf(
    paste(
      "Pairwise disagreement: %s; 95%% interval [%s, %s],",
      "worst-case upper bound %s, best-case lower bound %s."
    ),
    fmt_num(stability$pairwise_disagreement),
    fmt_num(stability$pairwise_disagreement_conf_low),
    fmt_num(stability$pairwise_disagreement_conf_high),
    fmt_num(worst), fmt_num(best)
  )
  md <- paste(format(rep, output = "md"), collapse = "\n")
  expect_match(md, line, fixed = TRUE)
  expect_match(plain_console(rep), line, fixed = TRUE)

  # No item with two judgments: no bound to show, and none is invented; nor
  # is a bound that is not a number.
  for (value in list(NA_real_, TRUE, c(0.1, 0.2), -0.1, 1.5)) {
    bad <- stability
    bad["pairwise_disagreement_worst_case_high"] <- list(value)
    bad["pairwise_disagreement_best_case_low"] <- list(value)
    md <- format(report(render_cal(), stability = bad), output = "md")
    expect_none(grepl("worst-case", md, fixed = TRUE))
    expect_none(grepl("best-case", md, fixed = TRUE))
  }
})

test_that("evidence the report validates carries every field it requires", {
  bias <- render_bias()
  position <- probe_position(
    tibble::tibble(a = paste("A", 1:20), b = paste("B", 1:20)),
    function(d) tibble::tibble(.item_id = d$.item_id, label = "A"),
    "a", "b", seed = 1, audit_id = "render-bundle"
  )
  stability <- render_stability()
  expect_named(bias, names(bias_row_prototype))
  expect_named(position, names(bias_row_prototype))
  expect_named(combine_bias(bias, position), names(bias_row_prototype))
  expect_identical(
    names(stability)[seq_along(stability_fields)], stability_fields
  )

  # A missing field is named, so the caller learns which one.
  bias$n_discordant <- NULL
  expect_error(
    report(render_cal(), bias = bias), "n_discordant",
    class = "yourhonor_input_error"
  )
  stability$stability_status <- NULL
  expect_error(
    report(render_cal(), stability = stability), "stability_status",
    class = "yourhonor_input_error"
  )
  # The bound the policy assesses is required like any field the report
  # shows, so evidence without it is refused rather than half-assessed.
  stability <- render_stability()
  stability$pairwise_disagreement_worst_case_high <- NULL
  expect_error(
    report(render_cal(), stability = stability),
    "pairwise_disagreement_worst_case_high", class = "yourhonor_input_error"
  )
})

test_that("policies print their gates", {
  minimal <- render_policy(coefficient = NULL)
  expect_snapshot(print(minimal))
  expect_invisible(print(minimal))
  expect_identical(withVisible(print(minimal))$value, minimal)

  full <- render_policy(
    min_recall = c(I = 0.8, C = 0.85), min_precision = 0.7,
    min_human_reliability_lower = 0.6, max_abstention_rate = 0.05,
    max_pairwise_disagreement = 0.1, min_stability_n = 150,
    min_stability_runs = 3, max_stability_missing_rate = 0.01,
    required_probes = c("verbosity", "position"), min_probe_n = 20,
    max_probe_flip_rate = 0.1, max_probe_directional_rate = 0.1,
    max_probe_missing_rate = 0.01, conf_level = 0.9
  )
  expect_snapshot(print(full))
})

test_that("interval labels keep the confidence level's digits", {
  expect_identical(conf_level_label(0.95), "95% ")
  expect_identical(conf_level_label(0.975), "97.5% ")
  expect_identical(conf_level_label(0.99999), "99.999% ")
  expect_identical(conf_level_label(NULL), "")
})

test_that("a list section with no items reads as not available", {
  block <- block_list("Per-class evidence", character(0))
  expect_identical(block[[1]]$type, "para")
  expect_identical(block[[1]]$text, "Per-class evidence: not available.")
})

test_that("several missing evidence fields are named in the plural", {
  bias <- render_bias()
  bias$n_discordant <- NULL
  bias$p_value <- NULL
  expect_error(
    report(render_cal(), bias = bias),
    "lacks required audit fields n_discordant and p_value",
    class = "yourhonor_input_error"
  )
})
