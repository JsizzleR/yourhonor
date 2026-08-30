#' Test-retest stability of a judge
#'
#' Re-grades the same items and reports chance-corrected agreement plus mean
#' pairwise disagreement. `any_flip_rate_at_n_runs` is retained as a descriptive
#' quantity but must not be compared across different `n_runs`, because it rises
#' mechanically as more opportunities to flip are observed.
#'
#' @param data A data frame of items to grade.
#' @param judge A judge function `(data) -> labels`.
#' @param input_cols Columns visible to `judge`. Required so labels and other
#'   audit-only fields are not passed through accidentally.
#' @param id_col Optional unique source identifier. It is validated but is not
#'   exposed to the judge unless explicitly included in `input_cols`.
#' @param n_runs Number of repeated gradings; must be at least two.
#' @param seed Optional R random seed. This does not force deterministic behavior
#'   from a remote model provider.
#' @param label_levels Optional complete label universe.
#' @param weights Agreement weights passed to Gwet's coefficient.
#' @param conf_level Bootstrap confidence level.
#' @param n_boot Number of item-bootstrap resamples.
#' @param audit_id Optional evidence-bundle identifier. Use the same value as
#'   the related calibration and bias probes.
#'
#' @return A `yourhonor_stability` object with raw runs, per-item evidence,
#'   missingness, pairwise disagreement, Gwet agreement, and intervals.
#'   `stability_status` explains why chance-corrected agreement is unavailable;
#'   in particular, one observed category is `insufficient_variation`, not
#'   perfect AC1/AC2. The pairwise-disagreement interval remains available.
#' @export
#' @examples
#' noisy_judge <- function(d) sample(c("C", "P"), nrow(d), replace = TRUE)
#' data <- tibble::tibble(input = paste("answer", 1:10))
#' test_retest(data, noisy_judge, input_cols = "input",
#'             n_runs = 4, n_boot = 100, seed = 1)
test_retest <- function(data,
                        judge,
                        input_cols = NULL,
                        id_col = NULL,
                        n_runs = 5,
                        seed = NULL,
                        label_levels = NULL,
                        weights = c("unweighted", "linear", "quadratic", "ordinal"),
                        conf_level = 0.95,
                        n_boot = 1000,
                        audit_id = NULL) {
  if (!is.function(judge)) {
    cli::cli_abort("{.arg judge} must be a function to re-run for stability.")
  }
  n_runs <- check_count(n_runs, "n_runs", minimum = 2L)
  n_boot <- check_count(n_boot, "n_boot")
  audit_id <- check_audit_id(audit_id)
  conf_level <- check_probability(conf_level, "conf_level", open = TRUE)
  weights <- rlang::arg_match(weights)
  if (weights != "unweighted" && is.null(label_levels)) {
    cli::cli_abort(
      "{.arg label_levels} is required for weighted stability; ordinal order cannot be inferred."
    )
  }
  data <- tibble::as_tibble(data)
  if (nrow(data) < 1L) cli::cli_abort("{.arg data} must contain at least one item.")
  if (!is.null(seed)) withr::local_seed(seed)
  judge_input <- redacted_judge_input(data, input_cols, id_col = id_col)

  runs <- vapply(
    seq_len(n_runs),
    function(i) grade_blind(judge, judge_input, label_levels)$label,
    character(nrow(data))
  )
  runs <- matrix(runs, nrow = nrow(data), ncol = n_runs)
  summarize_stability_runs(
    runs = runs,
    label_levels = label_levels,
    weights = weights,
    conf_level = conf_level,
    n_boot = n_boot,
    audit_id = audit_id
  )
}

summarize_stability_runs <- function(runs,
                                     label_levels = NULL,
                                     weights = "unweighted",
                                     conf_level = 0.95,
                                     n_boot = 1000,
                                     audit_id = NULL) {
  runs <- as.matrix(runs)
  storage.mode(runs) <- "character"
  if (nrow(runs) < 1L || ncol(runs) < 2L) {
    cli::cli_abort("Stability ratings need at least one item and two runs.")
  }
  n_runs <- ncol(runs)
  if (all(is.na(runs))) {
    cli::cli_abort("Every stability judgment was missing.")
  }
  label_levels <- resolve_label_levels(
    as.data.frame(runs, stringsAsFactors = FALSE),
    label_levels,
    allow_single = TRUE
  )

  observed_count <- apply(runs, 1, function(z) sum(!is.na(z)))
  per_item <- tibble::tibble(
    .row = seq_len(nrow(runs)),
    modal = apply(runs, 1, modal_value),
    n_observed = observed_count,
    n_missing = n_runs - observed_count,
    n_distinct = apply(runs, 1, function(z) length(unique(stats::na.omit(z)))),
    flipped = apply(runs, 1, function(z) {
      z <- stats::na.omit(z)
      if (length(z) < 2L) NA else length(unique(z)) > 1L
    }),
    tie = apply(runs, 1, is_tie)
  )

  item_disagreement <- item_pairwise_disagreement(runs)
  per_item$pairwise_disagreement <- item_disagreement
  pairwise_disagreement <- if (all(is.na(item_disagreement))) {
    NA_real_
  } else {
    mean(item_disagreement, na.rm = TRUE)
  }
  pairwise <- if (is.na(pairwise_disagreement)) NA_real_ else 1 - pairwise_disagreement
  chance <- chance_corrected_stability(runs, weights, label_levels)
  chance_status <- chance_corrected_stability_status(runs, chance)
  boot <- bootstrap_stability(runs, n_boot, conf_level, weights, label_levels)
  any_flip <- if (all(is.na(per_item$flipped))) NA_real_ else mean(per_item$flipped, na.rm = TRUE)
  stability_boundary_degenerate <- identical(chance_status, "ok") &&
    is.finite(chance) && chance >= 1 - sqrt(.Machine$double.eps) &&
    is.finite(boot$stability$se) &&
    abs(boot$stability$se) <= sqrt(.Machine$double.eps) &&
    is.finite(boot$stability$conf_low) &&
    is.finite(boot$stability$conf_high) &&
    abs(boot$stability$conf_high - boot$stability$conf_low) <=
      sqrt(.Machine$double.eps)

  structure(
    list(
      per_item = per_item,
      runs = runs,
      any_flip_rate_at_n_runs = any_flip,
      flip_rate = any_flip,
      pairwise_disagreement = pairwise_disagreement,
      pairwise_disagreement_conf_low = boot$pairwise$conf_low,
      pairwise_disagreement_conf_high = boot$pairwise$conf_high,
      stability = chance,
      stability_conf_low = boot$stability$conf_low,
      stability_conf_high = boot$stability$conf_high,
      stability_status = chance_status,
      stability_boundary_degenerate = stability_boundary_degenerate,
      percent_agreement = pairwise,
      missing_rate = mean(is.na(runs)),
      n_runs = n_runs,
      n_items = nrow(runs),
      n_items_scored = sum(is.finite(item_disagreement)),
      label_levels = label_levels,
      weights = weights,
      stability_coefficient = if (weights == "unweighted") "gwet_ac1" else "gwet_ac2",
      conf_level = conf_level,
      n_boot = n_boot,
      pairwise_n_boot_effective = boot$pairwise$n_effective,
      stability_n_boot_effective = boot$stability$n_effective,
      n_boot_effective = min(boot$pairwise$n_effective, boot$stability$n_effective),
      pairwise_ci_reliable = boot$pairwise$ci_reliable &&
        boot$pairwise$n_effective >= ceiling(0.8 * n_boot),
      stability_ci_reliable = boot$stability$ci_reliable &&
        boot$stability$n_effective >= ceiling(0.8 * n_boot) &&
        !stability_boundary_degenerate,
      ci_reliable = boot$pairwise$ci_reliable && boot$stability$ci_reliable &&
        min(boot$pairwise$n_effective, boot$stability$n_effective) >=
          ceiling(0.8 * n_boot) && !stability_boundary_degenerate,
      audit_id = audit_id
    ),
    class = "yourhonor_stability"
  )
}

#' @export
print.yourhonor_stability <- function(x, ...) {
  cli::cli_h2("yourhonor test-retest stability")
  cli::cli_text(
    "{x$n_runs} runs - pairwise disagreement {fmt_num(x$pairwise_disagreement)} ",
    "[{fmt_num(x$pairwise_disagreement_conf_low)}, ",
    "{fmt_num(x$pairwise_disagreement_conf_high)}]."
  )
  if (identical(x$stability_status, "ok")) {
    cli::cli_text(
      "{x$stability_coefficient}: {fmt_num(x$stability)} ",
      "[{fmt_num(x$stability_conf_low)}, {fmt_num(x$stability_conf_high)}]."
    )
  } else {
    cli::cli_text(
      "{x$stability_coefficient}: not estimable ({x$stability_status})."
    )
  }
  cli::cli_text(
    "Any-flip rate at exactly {x$n_runs} runs: ",
    "{fmt_num(x$any_flip_rate_at_n_runs)}; missing judgments: {fmt_num(x$missing_rate)}."
  )
  invisible(x)
}

modal_value <- function(z) {
  z <- z[!is.na(z)]
  if (length(z) == 0L) return(NA_character_)
  tab <- table(z)
  top <- names(tab)[tab == max(tab)]
  if (length(top) > 1L) NA_character_ else top
}

is_tie <- function(z) {
  z <- z[!is.na(z)]
  if (length(z) == 0L) return(FALSE)
  tab <- table(z)
  sum(tab == max(tab)) > 1L
}

chance_corrected_stability <- function(runs,
                                       weights = "unweighted",
                                       label_levels = NULL) {
  if (ncol(runs) < 2L || nrow(runs) < 1L) return(NA_real_)
  runs <- runs[rowSums(!is.na(runs)) >= 2L, , drop = FALSE]
  if (nrow(runs) < 2L) return(NA_real_)
  runs <- runs[, colSums(!is.na(runs)) > 0L, drop = FALSE]
  if (ncol(runs) < 2L) return(NA_real_)
  if (length(unique(stats::na.omit(as.vector(runs)))) < 2L) {
    return(NA_real_)
  }
  tryCatch(
    as.numeric(irrCAC::gwet.ac1.raw(
      as.data.frame(runs, stringsAsFactors = FALSE),
      weights = weights,
      categ.labels = label_levels
    )$est$coeff.val),
    error = function(e) NA_real_
  )
}

chance_corrected_stability_status <- function(runs, estimate) {
  usable <- runs[rowSums(!is.na(runs)) >= 2L, , drop = FALSE]
  if (nrow(usable) < 2L) return("insufficient_items")
  usable <- usable[, colSums(!is.na(usable)) > 0L, drop = FALSE]
  if (ncol(usable) < 2L) return("insufficient_runs")
  if (length(unique(stats::na.omit(as.vector(usable)))) < 2L) {
    return("insufficient_variation")
  }
  if (!is.finite(estimate)) return("undefined")
  "ok"
}

pairwise_agreement <- function(runs) {
  disagreement <- item_pairwise_disagreement(runs)
  if (all(is.na(disagreement))) NA_real_ else 1 - mean(disagreement, na.rm = TRUE)
}

item_pairwise_disagreement <- function(runs) {
  apply(runs, 1, function(values) {
    values <- values[!is.na(values)]
    if (length(values) < 2L) return(NA_real_)
    pairs <- utils::combn(values, 2L)
    mean(pairs[1, ] != pairs[2, ])
  })
}

wilson_interval <- function(probability, n, conf_level) {
  if (!is.finite(probability) || n < 1L) return(c(NA_real_, NA_real_))
  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  z2 <- z^2
  denominator <- 1 + z2 / n
  center <- (probability + z2 / (2 * n)) / denominator
  half <- z * sqrt(probability * (1 - probability) / n + z2 / (4 * n^2)) /
    denominator
  pmax(0, pmin(1, c(center - half, center + half)))
}

bootstrap_stability <- function(runs,
                                n_boot,
                                conf_level,
                                weights,
                                label_levels) {
  n <- nrow(runs)
  pairwise <- numeric(n_boot)
  stability <- numeric(n_boot)
  for (i in seq_len(n_boot)) {
    samp <- runs[sample.int(n, n, replace = TRUE), , drop = FALSE]
    pa <- pairwise_agreement(samp)
    pairwise[[i]] <- if (is.na(pa)) NA_real_ else 1 - pa
    stability[[i]] <- chance_corrected_stability(samp, weights, label_levels)
  }
  pairwise_ci <- bootstrap_quantiles(pairwise, conf_level)
  item_disagreement <- item_pairwise_disagreement(runs)
  item_disagreement <- item_disagreement[is.finite(item_disagreement)]
  boundary <- wilson_interval(
    if (length(item_disagreement) == 0L) NA_real_ else mean(item_disagreement),
    length(item_disagreement), conf_level
  )
  if (all(is.finite(boundary))) {
    pairwise_ci$conf_low <- min(pairwise_ci$conf_low, boundary[[1]], na.rm = TRUE)
    pairwise_ci$conf_high <- max(pairwise_ci$conf_high, boundary[[2]], na.rm = TRUE)
  }
  list(
    pairwise = pairwise_ci,
    stability = bootstrap_quantiles(stability, conf_level)
  )
}
