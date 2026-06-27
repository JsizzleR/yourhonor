#' Chance-corrected agreement between a judge and humans
#'
#' Reports how closely a set of judge labels agrees with human labels, using
#' the agreement coefficients appropriate for categorical ratings. All
#' coefficients are chance-corrected; confidence intervals come from a
#' nonparametric bootstrap over items so that every coefficient is reported on
#' the same footing.
#'
#' The coefficients are reported together because they disagree in informative
#' ways. Cohen's kappa is the familiar baseline but is unstable when one label
#' dominates (the "high-agreement, low-kappa" paradox). Gwet's AC1 is robust to
#' that prevalence problem, and Krippendorff's alpha generalizes across scales
#' and tolerates missing data. When AC1 sits well above kappa, your labels are
#' skewed and kappa is understating real agreement.
#'
#' @details `cohen_kappa` is computed with `irrCAC::conger.kappa.raw()`, which
#'   equals Cohen's kappa for two raters. The Landis & Koch interpretation bands
#'   are a kappa-derived heuristic and are reused for all coefficients only as a
#'   rough guide; alpha and AC1 have their own conventions.
#'
#' @param x Either an `yourhonor_calibration` object (in which case the stored
#'   agreement table is returned) or a two-column data frame / tibble of
#'   ratings. If columns named `human` and `judge` are present they are used;
#'   otherwise the first two columns are taken as the two raters. Rows with a
#'   missing label in either column are dropped with a warning.
#' @param coefficients Character vector of coefficients to compute. Any of
#'   `"cohen_kappa"`, `"krippendorff_alpha"`, `"gwet_ac1"`.
#' @param conf_level Width of the bootstrap confidence interval.
#' @param n_boot Number of bootstrap resamples used for the interval.
#' @param seed Optional integer seed for reproducible bootstrap intervals. The
#'   caller's random state is restored on exit.
#'
#' @return A tibble with one row per coefficient and columns `coefficient`,
#'   `estimate`, `conf_low`, `conf_high`, `se`, `n_items`, `n_raters` and a
#'   plain-language `interpretation`.
#'
#' @seealso [calibrate_judge()] which calls this on a judge and a gold set.
#' @export
#' @examples
#' ratings <- data.frame(
#'   human = c("C", "C", "P", "I", "C", "P", "I", "C"),
#'   judge = c("C", "C", "C", "I", "C", "P", "P", "C")
#' )
#' agreement(ratings, n_boot = 200, seed = 1)
agreement <- function(x,
                      coefficients = yourhonor_coefficients,
                      conf_level = 0.95,
                      n_boot = 2000,
                      seed = NULL) {
  if (inherits(x, "yourhonor_calibration")) {
    return(x$agreement)
  }
  coefficients <- rlang::arg_match(coefficients, yourhonor_coefficients, multiple = TRUE)
  ratings <- as_ratings(x)
  compute_agreement(ratings, coefficients, conf_level, n_boot, seed)
}

# Per-coefficient estimate + bootstrap CI over a clean ratings frame. Works for
# two raters (the public judge-vs-human path) or for the multi-rater human gold
# matrix used to compute the human-agreement ceiling.
compute_agreement <- function(ratings, coefficients, conf_level, n_boot, seed) {
  if (!is.null(seed)) withr::local_seed(seed)
  rows <- lapply(coefficients, function(co) {
    est <- coeff_point(ratings, co)
    ci <- boot_ci(ratings, co, n_boot = n_boot, conf_level = conf_level)
    tibble::tibble(
      coefficient = co,
      estimate = est,
      conf_low = ci$conf_low,
      conf_high = ci$conf_high,
      se = ci$se,
      n_items = nrow(ratings),
      n_raters = ncol(ratings),
      interpretation = interpret_agreement(est)
    )
  })
  do.call(vctrs::vec_rbind, rows)
}

# Coerce assorted inputs to a clean two-column character data frame of ratings.
as_ratings <- function(x) {
  if (inherits(x, "yourhonor_calibration")) {
    return(x$ratings)
  }
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  if (all(c("human", "judge") %in% names(x))) {
    x <- x[, c("human", "judge"), drop = FALSE]
  } else if (ncol(x) >= 2) {
    x <- x[, 1:2, drop = FALSE]
  } else {
    cli::cli_abort("Ratings need at least two columns (e.g. {.field human} and {.field judge}).")
  }
  x[] <- lapply(x, as.character)

  keep <- stats::complete.cases(x)
  if (any(!keep)) {
    cli::cli_warn("Dropped {sum(!keep)} row{?s} with a missing label.")
    x <- x[keep, , drop = FALSE]
  }
  if (nrow(x) < 2) {
    cli::cli_abort("Need at least 2 complete rated items to estimate agreement.")
  }
  x
}

# Point estimate of a single coefficient via {irrCAC}. For two raters Conger's
# kappa is identical to Cohen's kappa, so it backs `cohen_kappa` here.
coeff_point <- function(ratings, coefficient) {
  if (coefficient == "cohen_kappa" && ncol(ratings) != 2) {
    cli::cli_abort(
      "{.val cohen_kappa} is defined for exactly two raters; got {ncol(ratings)}."
    )
  }
  val <- switch(
    coefficient,
    cohen_kappa = irrCAC::conger.kappa.raw(ratings)$est$coeff.val,
    krippendorff_alpha = irrCAC::krippen.alpha.raw(ratings)$est$coeff.val,
    gwet_ac1 = irrCAC::gwet.ac1.raw(ratings)$est$coeff.val,
    cli::cli_abort("Unknown coefficient {.val {coefficient}}.")
  )
  as.numeric(val)
}

# Nonparametric bootstrap CI by resampling subjects (rows). A resample that
# collapses to a single label class leaves the coefficient undefined (irrCAC
# would return a spurious 1), so such draws are dropped rather than counted.
boot_ci <- function(ratings, coefficient, n_boot, conf_level) {
  n <- nrow(ratings)
  stat <- vapply(seq_len(n_boot), function(i) {
    samp <- ratings[sample.int(n, n, replace = TRUE), , drop = FALSE]
    if (length(unique(unlist(samp, use.names = FALSE))) < 2) {
      return(NA_real_)
    }
    tryCatch(coeff_point(samp, coefficient), error = function(e) NA_real_)
  }, numeric(1))
  alpha <- (1 - conf_level) / 2
  qs <- stats::quantile(stat, c(alpha, 1 - alpha), na.rm = TRUE, names = FALSE)
  list(conf_low = qs[[1]], conf_high = qs[[2]], se = stats::sd(stat, na.rm = TRUE))
}

# Landis & Koch (1977) bands. A kappa-derived heuristic; right = TRUE so that
# boundary values (0.40, 0.60, 0.80) fall in the lower, conventional band.
interpret_agreement <- function(value) {
  if (length(value) != 1 || is.na(value)) {
    return(NA_character_)
  }
  breaks <- c(-Inf, 0.0, 0.2, 0.4, 0.6, 0.8, Inf)
  labels <- c("poor", "slight", "fair", "moderate", "substantial", "almost perfect")
  as.character(cut(value, breaks = breaks, labels = labels, right = TRUE))
}
