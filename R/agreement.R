#' Chance-corrected agreement between a judge and humans
#'
#' Reports how closely a set of judge labels agrees with human labels. Confidence
#' intervals use a nonparametric item bootstrap. Declared label levels are held
#' fixed across bootstrap draws so rare categories do not silently disappear from
#' the estimand.
#'
#' Agreement is descriptive evidence, not a universal fitness gate. In
#' particular, a high aggregate coefficient can coexist with zero recall on a
#' rare, important class. Use [report()] with an explicit
#' [acceptance_policy()] and inspect [confusion_metrics()] before making a
#' deployment decision.
#'
#' @param x Either a `yourhonor_calibration` object or a data frame / tibble of
#'   ratings. If columns named `human` and `judge` are present they are used;
#'   otherwise all supplied columns are treated as raters. Rows with fewer than
#'   two observed ratings do not contribute, while partially observed
#'   multi-rater rows remain available to coefficients that support them.
#' @param coefficients Character vector containing any of `"cohen_kappa"`,
#'   `"krippendorff_alpha"`, `"gwet_ac1"`, and `"gwet_ac2"`. AC1 is
#'   unweighted; AC2 is its weighted counterpart.
#' @param conf_level Confidence level for the bootstrap interval.
#' @param n_boot Positive number of bootstrap resamples.
#' @param seed Optional integer seed. The caller's random state is restored.
#' @param label_levels Complete label universe in its intended order. Supplying
#'   this is strongly recommended for ordinal scales and rare classes.
#' @param weights Agreement weights. `"unweighted"` treats labels as nominal;
#'   `"linear"`, `"quadratic"`, or `"ordinal"` use the order in
#'   `label_levels`.
#'
#' @return A tibble with point estimates, intervals, sample sizes, effective
#'   bootstrap counts, an explicit computation `status`, and a
#'   `boundary_degenerate` flag. A perfect finite sample can produce a
#'   zero-width nonparametric bootstrap; that interval is flagged unreliable
#'   rather than treated as proof of population-perfect agreement.
#' @export
#' @examples
#' ratings <- data.frame(
#'   human = c("C", "C", "P", "I", "C", "P", "I", "C"),
#'   judge = c("C", "C", "C", "I", "C", "P", "P", "C")
#' )
#' agreement(ratings, n_boot = 200, seed = 1,
#'           label_levels = c("I", "P", "C"), weights = "ordinal")
agreement <- function(x,
                      coefficients = yourhonor_coefficients,
                      conf_level = 0.95,
                      n_boot = 2000,
                      seed = NULL,
                      label_levels = NULL,
                      weights = c("unweighted", "linear", "quadratic", "ordinal")) {
  stored_request <- inherits(x, "yourhonor_calibration") &&
    missing(coefficients) && missing(conf_level) && missing(n_boot) &&
    missing(seed) && missing(label_levels) && missing(weights)
  if (stored_request) return(x$agreement)

  coefficients_missing <- missing(coefficients)
  coefficients <- rlang::arg_match(coefficients, supported_coefficients, multiple = TRUE)
  weights_missing <- missing(weights)
  weights <- rlang::arg_match(weights)
  conf_level <- check_probability(conf_level, "conf_level", open = TRUE)
  n_boot <- check_count(n_boot, "n_boot")

  if (inherits(x, "yourhonor_calibration")) {
    if (is.null(label_levels)) label_levels <- x$label_levels
    if (weights_missing && !is.null(x$weights)) weights <- x$weights
  }
  coefficients <- resolve_gwet_coefficient(
    coefficients, weights, coefficients_missing, "coefficients"
  )
  if (weights != "unweighted" && is.null(label_levels)) {
    cli::cli_abort(
      "{.arg label_levels} is required for weighted agreement; ordinal order cannot be inferred."
    )
  }
  ratings <- as_ratings(x)
  label_levels <- resolve_label_levels(ratings, label_levels, allow_single = TRUE)
  compute_agreement(
    ratings, coefficients, conf_level, n_boot, seed,
    label_levels = label_levels, weights = weights
  )
}

compute_agreement <- function(ratings,
                              coefficients,
                              conf_level,
                              n_boot,
                              seed,
                              label_levels = NULL,
                              weights = "unweighted") {
  conf_level <- check_probability(conf_level, "conf_level", open = TRUE)
  n_boot <- check_count(n_boot, "n_boot")
  ratings <- as.data.frame(ratings, stringsAsFactors = FALSE)
  label_levels <- resolve_label_levels(ratings, label_levels, allow_single = TRUE)
  ratings[] <- lapply(ratings, as.character)
  if (!is.null(seed)) withr::local_seed(seed)

  rows <- lapply(coefficients, function(co) {
    eligible <- rowSums(!is.na(ratings)) >= 2L
    usable <- ratings[eligible, , drop = FALSE]
    n_items <- nrow(usable)
    n_missing <- sum(!stats::complete.cases(ratings))
    variation <- length(observed_categories(usable))

    status <- "ok"
    estimate <- NA_real_
    ci <- list(
      conf_low = NA_real_, conf_high = NA_real_, se = NA_real_,
      n_effective = 0L, ci_reliable = FALSE
    )

    if (n_items < 2L) {
      status <- "insufficient_items"
    } else if (variation < 2L) {
      status <- "insufficient_variation"
    } else if (co == "cohen_kappa" && ncol(usable) != 2L) {
      status <- "invalid_rater_count"
    } else {
      estimate <- tryCatch(
        suppressWarnings(coeff_point(usable, co, weights, label_levels)),
        error = function(e) NA_real_
      )
      if (!is.finite(estimate)) {
        estimate <- NA_real_
        status <- "undefined"
      } else {
        ci <- boot_ci(
          usable, co, n_boot = n_boot, conf_level = conf_level,
          weights = weights, label_levels = label_levels
        )
      }
    }

    boundary_degenerate <- identical(status, "ok") && is.finite(estimate) &&
      estimate >= 1 - sqrt(.Machine$double.eps) &&
      is.finite(ci$se) && abs(ci$se) <= sqrt(.Machine$double.eps) &&
      is.finite(ci$conf_low) && is.finite(ci$conf_high) &&
      abs(ci$conf_high - ci$conf_low) <= sqrt(.Machine$double.eps)

    tibble::tibble(
      coefficient = co,
      estimate = estimate,
      conf_low = ci$conf_low,
      conf_high = ci$conf_high,
      se = ci$se,
      n_items = n_items,
      n_input = nrow(ratings),
      n_raters = ncol(ratings),
      n_missing_rows = n_missing,
      weights = weights,
      n_boot_effective = ci$n_effective,
      boundary_degenerate = boundary_degenerate,
      ci_reliable = ci$ci_reliable &&
        ci$n_effective >= ceiling(0.8 * n_boot) && !boundary_degenerate,
      status = status,
      interpretation = interpret_agreement(estimate)
    )
  })
  do.call(vctrs::vec_rbind, rows)
}

as_ratings <- function(x) {
  if (inherits(x, "yourhonor_calibration")) return(x$ratings)
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  if (all(c("human", "judge") %in% names(x))) {
    x <- x[, c("human", "judge"), drop = FALSE]
  }
  if (ncol(x) < 2L) {
    cli::cli_abort("Ratings need at least two columns (e.g. {.field human} and {.field judge}).")
  }
  if (nrow(x) < 2L) {
    cli::cli_abort("Need at least 2 rated items to estimate agreement.")
  }
  x
}

coeff_point <- function(ratings,
                        coefficient,
                        weights = "unweighted",
                        label_levels = NULL) {
  if (coefficient == "cohen_kappa" && ncol(ratings) != 2L) {
    cli::cli_abort(
      "{.val cohen_kappa} is defined for exactly two raters; got {ncol(ratings)}."
    )
  }
  val <- switch(
    coefficient,
    cohen_kappa = irrCAC::conger.kappa.raw(
      ratings, weights = weights, categ.labels = label_levels
    )$est$coeff.val,
    krippendorff_alpha = irrCAC::krippen.alpha.raw(
      ratings, weights = weights, categ.labels = label_levels
    )$est$coeff.val,
    gwet_ac1 = irrCAC::gwet.ac1.raw(
      ratings, weights = weights, categ.labels = label_levels
    )$est$coeff.val,
    gwet_ac2 = irrCAC::gwet.ac1.raw(
      ratings, weights = weights, categ.labels = label_levels
    )$est$coeff.val,
    cli::cli_abort("Unknown coefficient {.val {coefficient}}.")
  )
  as.numeric(val)
}

resolve_gwet_coefficient <- function(coefficients,
                                     weights,
                                     coefficients_missing = FALSE,
                                     arg = "coefficients") {
  if (weights == "unweighted") {
    if ("gwet_ac2" %in% coefficients) {
      cli::cli_abort("{.val gwet_ac2} requires non-default agreement weights.")
    }
    return(coefficients)
  }
  if ("gwet_ac1" %in% coefficients) {
    if (!coefficients_missing) {
      cli::cli_abort(paste0(
        "Weighted Gwet agreement is AC2, not AC1. Request {.val gwet_ac2} ",
        "or omit {.arg {arg}} to select it automatically."
      ))
    }
    coefficients[coefficients == "gwet_ac1"] <- "gwet_ac2"
  }
  coefficients
}

boot_ci <- function(ratings,
                    coefficient,
                    n_boot,
                    conf_level,
                    weights = "unweighted",
                    label_levels = NULL) {
  n <- nrow(ratings)
  stat <- vapply(seq_len(n_boot), function(i) {
    samp <- ratings[sample.int(n, n, replace = TRUE), , drop = FALSE]
    if (length(observed_categories(samp)) < 2L) return(NA_real_)
    val <- tryCatch(
      suppressWarnings(coeff_point(samp, coefficient, weights, label_levels)),
      error = function(e) NA_real_
    )
    if (is.finite(val)) val else NA_real_
  }, numeric(1))
  bootstrap_quantiles(stat, conf_level)
}

interpret_agreement <- function(value) {
  if (length(value) != 1L || is.na(value)) return(NA_character_)
  breaks <- c(-Inf, 0.0, 0.2, 0.4, 0.6, 0.8, Inf)
  labels <- c("poor", "slight", "fair", "moderate", "substantial", "almost perfect")
  as.character(cut(value, breaks = breaks, labels = labels, right = TRUE))
}
