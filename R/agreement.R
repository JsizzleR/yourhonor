#' Chance-corrected agreement between a judge and humans
#'
#' Reports how closely a set of judge labels agrees with human labels. Confidence
#' intervals use a nonparametric item bootstrap. The categories the sample
#' observed are held fixed across bootstrap draws so rare categories do not
#' silently disappear from the estimand.
#'
#' Chance agreement counts the categories some rater used, not every declared
#' level: Gwet's AC1 and AC2 divide by the number of categories, so a declared
#' level that no rater used would otherwise raise them on identical data
#' (Cohen's kappa and Krippendorff's alpha are unaffected by unused levels).
#' Unweighted, an unused declared level therefore changes no coefficient.
#' Weighted coefficients still take their weights from the full declared
#' scale, so an ordinal distance never depends on which levels were
#' observed. An unused level still changes the weights between the observed
#' levels, and every weighted chance term sums those weights: with linear
#' weights and levels 1 to 3 observed, declaring a fourth level can move
#' AC2's chance term from about 0.55 to 0.70 and so change AC2 itself.
#' Kappa's and alpha's chance terms move too. For a level beyond the observed
#' ones each weight scheme only rescales the distances between observed
#' levels, so their observed agreement moves with the chance term and their
#' estimates do not change (their `conf_low_exact` does; see below). A level
#' between observed ones can change those distances and the weighted
#' estimates with them.
#'
#' Agreement is descriptive evidence, not a universal fitness gate. In
#' particular, a high aggregate coefficient can coexist with zero recall on a
#' rare, important class. Use [report()] with an explicit
#' [acceptance_policy()] and inspect [confusion_metrics()] before making a
#' deployment decision.
#'
#' @param x Either a `yourhonor_calibration` object or a data frame / tibble of
#'   ratings with one column per rater (see `rater_cols`). Rows with fewer
#'   than two observed ratings do not contribute, while partially observed
#'   multi-rater rows remain available to coefficients that support them.
#'
#'   For a calibration object every argument left at its default is inherited
#'   from the calibration (`coefficients`, `conf_level`, `n_boot`, `seed`,
#'   `label_levels`, and `weights`), so `agreement(cal, n_boot = 50)` reuses the
#'   stored seed and confidence level. When nothing is overridden the stored
#'   agreement table is returned unchanged.
#' @param coefficients Character vector containing any of `"cohen_kappa"`,
#'   `"krippendorff_alpha"`, `"gwet_ac1"`, and `"gwet_ac2"`. `NULL` (the
#'   default) selects `"cohen_kappa"`, `"krippendorff_alpha"`, and
#'   `"gwet_ac1"`, replacing `"gwet_ac1"` with its weighted counterpart
#'   `"gwet_ac2"` when `weights` is not `"unweighted"`. Explicitly requesting
#'   `"gwet_ac1"` together with weights is an error rather than a silent
#'   substitution.
#' @param conf_level Confidence level for the bootstrap interval.
#' @param n_boot Positive number of bootstrap resamples. One set of item
#'   resamples is shared by every coefficient, so intervals from one call are
#'   computed on identical draws.
#' @param seed Optional integer seed. The caller's random state is restored.
#' @param label_levels Complete label universe in its intended order. Supplying
#'   this is strongly recommended for ordinal scales and rare classes. Labels
#'   are matched exactly: case and surrounding whitespace are significant, and
#'   empty or whitespace-only labels are rejected. Use `NA` for a missing or
#'   failed judgement.
#' @param weights Agreement weights. `"unweighted"` treats labels as nominal;
#'   `"linear"`, `"quadratic"`, and `"ordinal"` use the order in
#'   `label_levels` and are Gwet's weight schemes as implemented by
#'   \pkg{irrCAC} (`linear.weights()`, `quadratic.weights()`, and
#'   `ordinal.weights()`). Every weighted coefficient, including
#'   `"krippendorff_alpha"`, uses these schemes; `weights = "ordinal"` is
#'   Gwet's combinatorial ordinal weighting and is not Krippendorff's ordinal
#'   alpha, which weights by cumulative category frequencies.
#' @param rater_cols Optional character vector naming the rater columns of a
#'   ratings data frame. When `NULL`, columns named `human` and `judge` are
#'   used if both exist; otherwise every column is treated as a rater. A
#'   message (class `yourhonor_inferred_columns`) reports the choice whenever
#'   inference ignored columns or fell back to all columns; supply
#'   `rater_cols` to choose explicitly. Cannot be combined with a calibration
#'   object, which carries its own ratings.
#'
#' @return A tibble with one row per coefficient and columns
#'   `coefficient`, `estimate`, `conf_low`, `conf_high`, `se`, `n_items`,
#'   `conf_low_exact`, `n_input`, `n_raters`, `n_missing_rows`, `weights`,
#'   `n_boot_effective`, `boundary_degenerate`, `ci_reliable`, `status`, and
#'   `interpretation`.
#'
#'   `status` is `"ok"` when the estimate and interval were computed, or one
#'   of `"insufficient_items"`, `"insufficient_variation"`,
#'   `"insufficient_reference_variation"`, `"invalid_rater_count"`, and
#'   `"undefined"`. `"insufficient_reference_variation"` applies to a
#'   calibration object, whose human (consensus) reference must use at least
#'   two categories: against a single-class reference every chance-corrected
#'   coefficient is uninformative, and Gwet's AC1 in particular can read
#'   "almost perfect" while kappa reads zero. The rule applies to the full
#'   sample only: a bootstrap resample whose reference happens to hold one
#'   category still contributes its coefficient.
#'
#'   `conf_low_exact` is a resampling-free lower bound on the coefficient's
#'   own observed agreement `pa`, mapped onto its scale as
#'   `(lower - pe) / (1 - pe)` with the sample's chance term `pe` held
#'   fixed, then capped as described below.
#'   `pa` is the mean over items of each item's weighted pairwise agreement,
#'   so a near miss under weights earns its partial credit; Krippendorff's
#'   alpha's `pa` is `(1 - eps) * A + eps`, with `A` the mean weighted by
#'   each item's number of ratings `r` and `eps = 1 / sum(r)`, and its bound
#'   is on `A`. Each item's agreement is bounded by the declared
#'   weights' range, 0 to 1, not by the weights between the categories the
#'   sample happened to show: a rare, distant disagreement the sample missed
#'   would otherwise make the bound overshoot. Declaring a level beyond the
#'   observed ones therefore lowers kappa's and alpha's bound even where it
#'   leaves their estimates unchanged. The bound is
#'   one-sided at level `(1 + conf_level) / 2`, like the lower end of the
#'   bootstrap interval. With two raters and no weights each item agrees or
#'   not, and the bound is Clopper-Pearson's. Otherwise each item's agreement
#'   lies between 0 and 1, and the bound is the binomial stop-loss bound for
#'   a mean of such values, which is never looser than Hoeffding's; for alpha
#'   with three or more rater columns it bounds alpha's ratio of
#'   rating-weighted agreement by inverting Hoeffding's relative-entropy
#'   test, so missing ratings widen it. On a perfect sample every route gives
#'   Clopper-Pearson's `((1 - conf_level) / 2)^(1 / n)` for `n` items,
#'   except alpha with missing ratings, which is lower.
#'
#'   No sample's bound ranks above that of a perfect sample it can be
#'   corrected into: a correction gives every item whose ratings disagree
#'   one of its own ratings for all of them (in a calibration, the judge
#'   matching the human reference) and keeps the other items and the missing
#'   ratings, and the bound is capped at the worst such perfect sample's
#'   (for Gwet's coefficients, at or below it: their chance term is bounded
#'   from above rather than found exactly).
#'   Without the cap an error could rank above its own correction, because
#'   a rating moved into a rarer category, or into one the perfect sample
#'   never used, raises the chance disagreement `1 - pe` that divides the
#'   bound's margin. The cap costs most at small `n` with rare categories,
#'   where a rare category seen only in disagreements lets a correction drop
#'   it: in simulation with 20 to 30 items and end categories of 1% to 3%,
#'   the share of samples whose AC2 bound reached 0.6 fell from 12% to 1%
#'   in the worst scenario (three raters, end categories of 3%, 30 items),
#'   while coverage was unchanged; from 100 items it moved the mean bound by
#'   about 0.004 at most. Two imperfect samples keep no such guarantee:
#'   correcting an error can lower the estimate itself (when it removes the
#'   only rating in a category from the chance term), so no bound that
#'   tracks the estimate can always rise with a correction.
#'
#'   As items accumulate the bound approaches the estimate, at a rate of
#'   about `1 / sqrt(n)`, for every coefficient, weighting and number of
#'   raters, provided `1 - pe` stays away from 0, for the corrections too.
#'   For kappa and alpha that holds when the items the raters agree on use
#'   at least two categories in non-vanishing shares. Gwet's chance term for
#'   a correction always stays below 1, but on a wide quadratic scale the
#'   gap can be small (about 0.02 with six levels), so the cap can hold the
#'   bound down until a few thousand items.
#'
#'   It is exact (a finite-sample bound, not an asymptotic one) for observed
#'   agreement, not for the coefficient, because `pe` is itself estimated;
#'   in simulation (two and several raters, weighted and not, alpha with
#'   missing ratings, 30 to 1000 items) its coverage of the coefficient was
#'   at or above nominal, within simulation error. It does not depend on
#'   resampling, so it stays honest where the percentile bootstrap is
#'   optimistic: at small `n`, and on a perfect sample, whose bootstrap is a
#'   zero-width interval at 1. Because the bound on observed agreement is
#'   divided by `1 - pe`, it is weak for kappa and alpha on imbalanced
#'   labels, whose `pe` is large: at 90/10 prevalence it stays far below
#'   the estimate until a few hundred items (a kappa of 0.92 on 100 items
#'   averaged about 0.63 in simulation), and it can fall below -1 (kappa's
#'   floor is `-pe / (1 - pe)`). Gwet's AC1 has a small `pe` there and is affected
#'   far less. It never exceeds `estimate`, is reported for every
#'   coefficient with `status = "ok"`, and is `NA` otherwise. Policies
#'   assess the smaller of `conf_low` and `conf_low_exact`; see
#'   [acceptance_policy()].
#'
#'   `ci_reliable` is `TRUE` only
#'   when at least 30 resamples, and at least 80% of the requested resamples,
#'   produced a finite statistic, and the interval is not
#'   `boundary_degenerate`. With `n_boot` below 30 every interval is
#'   therefore unreliable. A perfect finite sample
#'   produces a zero-width nonparametric bootstrap, and that interval is
#'   flagged unreliable rather than treated as proof of population-perfect
#'   agreement; `conf_low_exact` is the usable lower bound there. The
#'   percentile bootstrap is also optimistic at small `n` (around 30 items or
#'   fewer), which is why policies also read `conf_low_exact`.
#'
#'   `interpretation` applies the Landis and Koch (1977) bands
#'   (`"poor"` below 0, then `"slight"`, `"fair"`, `"moderate"`,
#'   `"substantial"`, and `"almost perfect"` at 0.2 steps). Those bands were
#'   proposed for kappa; applied to alpha, AC1, or AC2 they are a heuristic
#'   reading aid, not a validated scale, and the same data can land in
#'   different bands under different coefficients.
#' @references
#' Gwet, K. L. (2014). *Handbook of Inter-Rater Reliability* (4th ed.).
#' Advanced Analytics, LLC.
#'
#' Landis, J. R., & Koch, G. G. (1977). The measurement of observer agreement
#' for categorical data. *Biometrics*, 33(1), 159-174.
#' @export
#' @examples
#' ratings <- data.frame(
#'   human = c("C", "C", "P", "I", "C", "P", "I", "C"),
#'   judge = c("C", "C", "C", "I", "C", "P", "P", "C")
#' )
#' agreement(ratings, n_boot = 200, seed = 1,
#'           label_levels = c("I", "P", "C"), weights = "ordinal",
#'           rater_cols = c("human", "judge"))
agreement <- function(x,
                      coefficients = NULL,
                      conf_level = 0.95,
                      n_boot = 2000,
                      seed = NULL,
                      label_levels = NULL,
                      weights = c("unweighted", "linear", "quadratic", "ordinal"),
                      rater_cols = NULL) {
  # Capture the "left at default" flags before any argument is reassigned;
  # `missing()` reports FALSE once a formal has been overwritten.
  inherit <- inherits(x, "yourhonor_calibration")
  at_default <- c(
    coefficients = is.null(coefficients),
    conf_level = missing(conf_level),
    n_boot = missing(n_boot),
    seed = missing(seed),
    label_levels = is.null(label_levels),
    weights = missing(weights)
  )
  if (inherit && !is.null(rater_cols)) {
    cli::cli_abort(
      "{.arg rater_cols} cannot be combined with a calibration object."
    )
  }
  if (inherit && all(at_default)) return(x$agreement)

  if (inherit) {
    if (at_default[["coefficients"]]) coefficients <- x$coefficients
    if (at_default[["conf_level"]]) conf_level <- x$conf_level
    if (at_default[["n_boot"]]) n_boot <- x$n_boot
    if (at_default[["seed"]]) seed <- x$seed
    if (at_default[["label_levels"]]) label_levels <- x$label_levels
    if (at_default[["weights"]] && !is.null(x$weights)) weights <- x$weights
  }
  weights <- rlang::arg_match(weights)
  # Stored coefficients were resolved under the calibration's weights, so
  # they are re-resolved automatically when only `weights` is overridden.
  coefficients <- resolve_coefficients(
    coefficients, weights, inherited = inherit && at_default[["coefficients"]]
  )
  conf_level <- check_probability(conf_level, open = TRUE)
  n_boot <- check_count(n_boot)

  if (weights != "unweighted" && is.null(label_levels)) {
    cli::cli_abort(
      "{.arg label_levels} is required for weighted agreement; ordinal order cannot be inferred."
    )
  }
  ratings <- as_ratings(x, rater_cols)
  if (length(observed_categories(ratings)) == 0L) {
    cli::cli_abort("{.arg x} contains no non-missing ratings.")
  }
  label_levels <- resolve_label_levels(ratings, label_levels, allow_single = TRUE, arg = "x")
  compute_agreement(
    ratings, coefficients, conf_level, n_boot, seed,
    label_levels = label_levels, weights = weights,
    reference = if (inherit) "human" else NULL
  )
}

# `NULL` means "the default set, adapted to the weights"; an explicit vector
# is validated as given so callers never get a coefficient they did not ask
# for. A set `inherited` from a calibration is that object's already-resolved
# choice, so its Gwet variant follows the weights in force rather than being
# treated as an explicit, possibly conflicting, request.
resolve_coefficients <- function(coefficients,
                                 weights,
                                 inherited = FALSE,
                                 arg = "coefficients",
                                 call = rlang::caller_env()) {
  auto <- is.null(coefficients)
  if (auto) {
    coefficients <- yourhonor_coefficients
  } else {
    coefficients <- rlang::arg_match(
      coefficients, supported_coefficients, multiple = TRUE,
      error_arg = arg, error_call = call
    )
    # A repeated name would yield identical rows that a policy cannot tell
    # apart; like a repeated label level, it is the caller's mistake to fix.
    if (anyDuplicated(coefficients)) {
      cli::cli_abort(
        paste0(
          "{.arg {arg}} must not repeat a coefficient: ",
          "{.val {unique(coefficients[duplicated(coefficients)])}}."
        ),
        call = call
      )
    }
  }
  if (inherited) {
    coefficients[coefficients == "gwet_ac2"] <- "gwet_ac1"
  }
  resolve_gwet_coefficient(
    coefficients, weights, auto = auto || inherited, call = call
  )
}

compute_agreement <- function(ratings,
                              coefficients,
                              conf_level,
                              n_boot,
                              seed,
                              label_levels = NULL,
                              weights = "unweighted",
                              reference = NULL,
                              call = rlang::caller_env()) {
  conf_level <- check_probability(conf_level, open = TRUE, call = call)
  n_boot <- check_count(n_boot, call = call)
  ratings <- as.data.frame(ratings, stringsAsFactors = FALSE)
  label_levels <- resolve_label_levels(
    ratings, label_levels, allow_single = TRUE, call = call
  )
  ratings[] <- lapply(ratings, as.character)
  if (!is.null(seed)) withr::local_seed(seed)

  usable <- ratings[rowSums(!is.na(ratings)) >= 2L, , drop = FALSE]
  n_items <- nrow(usable)
  n_missing <- sum(!stats::complete.cases(ratings))
  # Held fixed across bootstrap draws: a declared level no rater used would
  # otherwise move Gwet's AC1 and AC2 on identical data.
  chance_levels <- chance_categories(usable, label_levels)
  variation <- length(chance_levels)
  reference_variation <- if (is.null(reference)) {
    NA_integer_
  } else {
    length(observed_categories(usable[reference]))
  }

  # Status and point estimate come first; only coefficients with a defined
  # estimate take part in the bootstrap, so nothing is drawn when none has.
  status <- vapply(coefficients, function(co) {
    if (n_items < 2L) {
      "insufficient_items"
    } else if (variation < 2L) {
      "insufficient_variation"
    } else if (!is.na(reference_variation) && reference_variation < 2L) {
      "insufficient_reference_variation"
    } else if (co == "cohen_kappa" && ncol(usable) != 2L) {
      "invalid_rater_count"
    } else {
      "ok"
    }
  }, character(1))
  fits <- lapply(seq_along(coefficients), function(i) {
    if (status[[i]] != "ok") return(NULL)
    safe_coeff_fit(
      usable, coefficients[[i]], weights, label_levels, chance_levels
    )
  })
  estimate <- vapply(fits, function(fit) {
    if (is.null(fit)) NA_real_ else fit$value
  }, numeric(1))
  status[status == "ok" & is.na(estimate)] <- "undefined"
  draws <- bootstrap_coefficients(
    usable, unique(coefficients[status == "ok"]), n_boot, weights,
    label_levels, chance_levels
  )
  # Two rater columns, unweighted: every item's agreement is 0 or 1, so the
  # agreeing share has an exact binomial bound.
  binary_low <- if (ncol(usable) == 2L && weights == "unweighted") {
    exact_agreement_lower(usable, conf_level)
  }
  counts <- rowSums(!is.na(usable))

  rows <- lapply(seq_along(coefficients), function(i) {
    co <- coefficients[[i]]
    co_status <- status[[i]]
    co_estimate <- estimate[[i]]
    stat <- if (co_status == "ok") draws[[co]] else numeric(0)
    ci <- bootstrap_quantiles(stat, conf_level)
    boundary_degenerate <- is_boundary_degenerate(co_status, co_estimate, ci)
    exact_low <- if (co_status == "ok") {
      low <- observed_agreement_lower(
        fits[[i]]$agreement, co, counts, ncol(usable), conf_level, binary_low
      )
      # No sample ranks above a perfect sample it can be corrected into.
      perfect_low <- perfect_sample_lower(
        usable, co, weights, label_levels, counts, conf_level
      )
      # irrCAC rounds the estimate to five decimals; the bound never exceeds it.
      min(chance_mapped(low, fits[[i]]$chance), perfect_low, co_estimate)
    } else {
      NA_real_
    }

    tibble::tibble(
      coefficient = co,
      estimate = co_estimate,
      conf_low = ci$conf_low,
      conf_high = ci$conf_high,
      conf_low_exact = exact_low,
      se = ci$se,
      n_items = n_items,
      n_input = nrow(ratings),
      n_raters = ncol(ratings),
      n_missing_rows = n_missing,
      weights = weights,
      n_boot_effective = ci$n_effective,
      boundary_degenerate = boundary_degenerate,
      ci_reliable = ci_is_reliable(ci, n_boot, boundary_degenerate),
      status = co_status,
      interpretation = interpret_agreement(co_estimate)
    )
  })
  do.call(vctrs::vec_rbind, rows)
}

# One item resample per iteration, evaluated for every coefficient: sharing
# the draw keeps the coefficients' intervals comparable, and drawing inside
# the loop avoids holding an n-by-n_boot index matrix. A resample that
# observes a single category is left NA for all of them. A resample whose
# reference holds one category is kept: the coefficient is still defined
# there, and dropping it would condition the interval on the draws that
# happen to look informative.
bootstrap_coefficients <- function(ratings,
                                   coefficients,
                                   n_boot,
                                   weights,
                                   label_levels,
                                   chance_levels = label_levels) {
  draws <- rlang::set_names(
    lapply(coefficients, function(co) rep(NA_real_, n_boot)), coefficients
  )
  if (length(coefficients) == 0L) return(draws)
  n <- nrow(ratings)
  for (i in seq_len(n_boot)) {
    samp <- ratings[sample.int(n, n, replace = TRUE), , drop = FALSE]
    if (length(observed_categories(samp)) < 2L) next
    for (co in coefficients) {
      draws[[co]][[i]] <- safe_coeff_point(
        samp, co, weights, label_levels, chance_levels
      )
    }
  }
  draws
}

# The categories a chance term counts: those observed on rows with at least
# two ratings, in declared order. agreement() and test_retest() share this
# rule, and each holds the result fixed across its bootstrap draws.
chance_categories <- function(ratings, label_levels) {
  usable <- ratings[rowSums(!is.na(ratings)) >= 2L, , drop = FALSE]
  intersect(label_levels, observed_categories(usable))
}

# Clopper-Pearson lower bound on the share of items whose observed ratings
# all agree. With two raters and no weights each item's agreement is 0 or 1,
# and that share is kappa's and AC1's `pa` and alpha's before its eps term.
exact_agreement_lower <- function(ratings, conf_level) {
  agree <- apply(ratings, 1, function(values) {
    length(unique(stats::na.omit(values))) == 1L
  })
  exact_binom_interval(sum(agree), length(agree), conf_level)[[1]]
}

# One-sided lower bound, at level `(1 + conf_level) / 2`, on a coefficient's
# own observed agreement `pa` as irrCAC computes it. Item i has weighted
# pairwise agreement a_i in [0, 1] over its r_i >= 2 ratings. Kappa (Conger)
# and Gwet's AC1 and AC2 average a_i over items; alpha's `pa` is
# (1 - eps) * sum(r a) / sum(r) + eps with eps = 1 / sum(r), whose
# population counterpart is the ratio E[r a] / E[r]. Items are taken as an
# independent sample, as the bootstrap takes them.
#
# Each a_i is bounded in [0, 1], the range of the declared weights. A
# narrower range taken from the categories the sample observed would not
# hold for the population: a rare, distant disagreement the sample missed
# makes those samples look perfect, and in simulation such a bound overshot
# the population coefficient in 15% of samples at a nominal 2.5%.
observed_agreement_lower <- function(pa,
                                     coefficient,
                                     counts,
                                     n_raters,
                                     conf_level,
                                     binary_low = NULL) {
  if (!is.null(binary_low)) return(binary_low)
  if (!is.finite(pa)) return(NA_real_)
  delta <- (1 - conf_level) / 2
  n <- length(counts)
  if (coefficient == "krippendorff_alpha") {
    eps <- 1 / sum(counts)
    pa <- (pa - eps) / (1 - eps)
  }
  if (coefficient == "krippendorff_alpha" && n_raters > 2L) {
    ratio_mean_lower(pa, counts, n_raters, delta)
  } else {
    # Two columns: every usable item has both ratings, so alpha's ratio is
    # a mean as well.
    bounded_mean_lower(pa, n, delta)
  }
}

# Lower bound on the mean of n independent values in [0, 1] from their sample
# mean. A [0, 1] value with mean mu is below Bernoulli(mu) in convex order, so
# the sum S is below Z ~ Binomial(n, mu), and for every c < t,
# P(S >= t) <= E[(Z - c)+] / (t - c). The bound is the mu at which the
# smallest of those reaches `delta`. It is never looser than Hoeffding's
# relative-entropy bound, and on a perfect sample it is delta^(1/n), the
# Clopper-Pearson bound for n successes.
bounded_mean_lower <- function(mean, n, delta) {
  mean <- min(max(mean, 0), 1)
  if (mean == 0) return(0)
  t <- n * mean
  stats::uniroot(
    function(mu) stop_loss_tail(mu, n, t) - delta,
    c(0, mean), tol = 1e-10
  )$root
}

# min(1, min over c < t of E[(Z - c)+] / (t - c)) for Z ~ Binomial(n, mu).
# The ratio is monotone between integers, so integer c in [0, t) suffice.
stop_loss_tail <- function(mu, n, t) {
  z <- 0:n
  p <- stats::dbinom(z, n, mu)
  tail_p <- rev(cumsum(rev(p)))
  tail_zp <- rev(cumsum(rev(z * p)))
  c <- seq_len(ceiling(t)) - 1L
  # E[(Z - c)+] sums z > c, which starts at index c + 2 of the tail sums.
  stop_loss <- tail_zp[c + 2L] - c * tail_p[c + 2L]
  min(1, stop_loss / (t - c))
}

# Lower bound on alpha's ratio E[r a] / E[r] by test inversion. With R the
# number of rater columns, fixed by design, y_i(m) = r_i (a_i - m) / R + m
# lies in [0, 1] and has mean m exactly when m is the ratio. Its sample mean
# is m + h (ratio - m) with h = mean(r) / R, and m is rejected when
# Hoeffding's relative-entropy bound puts that mean above m:
# n kl(m + h (ratio - m) || m) > log(1 / delta). kl is jointly convex and
# zero at the estimate, so the rejected values form one interval below the
# bound. Written this way the mean is exactly m at m = ratio, where kl must
# be 0 even when the ratio is 1.
ratio_mean_lower <- function(ratio, counts, n_raters, delta) {
  ratio <- min(max(ratio, 0), 1)
  if (ratio == 0) return(0)
  n <- length(counts)
  h <- mean(counts) / n_raters
  excess <- function(m) n * bernoulli_kl(m + h * (ratio - m), m) - log(1 / delta)
  low <- .Machine$double.eps
  if (excess(low) <= 0) return(0)
  stats::uniroot(excess, c(low, ratio), tol = 1e-10)$root
}

# Relative entropy between Bernoulli(x) and Bernoulli(q), with 0 log 0 = 0.
bernoulli_kl <- function(x, q) {
  x <- min(max(x, 0), 1)
  up <- if (x > 0) x * log(x / q) else 0
  down <- if (x < 1) (1 - x) * log((1 - x) / (1 - q)) else 0
  up + down
}

# The bound of the worst perfect sample `usable` could be corrected into. A
# correction gives every item whose ratings disagree one of its own ratings
# for all of them (the judge matching the human, say) and leaves the other
# items and the missing ratings as they are. Holding the sample's `pe` fixed
# would otherwise let an error rank above the correction it came from: a
# rating moved into a rarer category raises the chance disagreement
# `1 - pe` that divides the bound's margin, and one in a category the
# correction lacks changes the chance categories outright. A perfect sample
# has `pa` = 1, so its bound is `chance_mapped(L1, pe)` with `L1` the same
# route's bound at `pa` = 1, which depends only on the items and their
# rating counts; it falls as `pe` rises, so the worst correction is the one
# with the largest `pe`. `Inf` when no item disagrees.
perfect_sample_lower <- function(usable,
                                 coefficient,
                                 weights,
                                 label_levels,
                                 counts,
                                 conf_level) {
  chance <- perfect_chance_max(usable, coefficient, weights, label_levels)
  if (!is.finite(chance)) return(Inf)
  binary_low <- if (ncol(usable) == 2L && weights == "unweighted") {
    exact_binom_interval(nrow(usable), nrow(usable), conf_level)[[1]]
  }
  # irrCAC can report a perfect sample's alpha `pa` a few ulps below 1, so
  # the bound is taken just below it.
  low <- observed_agreement_lower(
    1 - 1e-12, coefficient, counts, ncol(usable), conf_level, binary_low
  )
  # A chance term of 1 has no coefficient; corrections keep two categories,
  # so these weights never reach it, and no cap is the safe reading.
  bound <- chance_mapped(low, chance)
  if (is.na(bound)) Inf else bound
}

# An upper bound on the chance term `pe` over the corrections of `ratings`
# that use at least two categories (one category has no coefficient). On a
# perfect sample `pe` depends only on each category's share `p`: of items
# for kappa and Gwet, of ratings for alpha. NA when no item disagrees. The
# small allowance keeps rounding from putting it below the exact maximum.
perfect_chance_max <- function(ratings,
                               coefficient,
                               weights,
                               label_levels,
                               max_vertices = 1e5) {
  ratings <- as.matrix(ratings)
  q <- length(label_levels)
  values <- lapply(seq_len(nrow(ratings)), function(i) {
    sort(unique(match(ratings[i, !is.na(ratings[i, ])], label_levels)))
  })
  mass <- if (coefficient == "krippendorff_alpha") {
    rowSums(!is.na(ratings))
  } else {
    rep(1, nrow(ratings))
  }
  agree <- lengths(values) == 1L
  if (all(agree)) return(NA_real_)
  base <- vapply(seq_len(q), function(k) {
    sum(mass[agree][unlist(values[agree]) == k])
  }, numeric(1))
  # Items with the same set of ratings form one group.
  disagree <- which(!agree)
  key <- vapply(values[disagree], paste, character(1), collapse = ",")
  groups <- lapply(split(disagree, key), function(items) {
    list(
      values = values[[items[[1]]]],
      mass = sum(mass[items]),
      min_mass = min(mass[items])
    )
  })
  weight_mat <- agreement_weight_matrix(weights, q)
  chance <- if (coefficient %in% c("gwet_ac1", "gwet_ac2")) {
    gwet_chance_max(base, groups, sum(mass), weight_mat)
  } else {
    quadratic_chance_max(base, groups, sum(mass), weight_mat, max_vertices)
  }
  chance + 64 * .Machine$double.eps
}

# Kappa (two raters, whose shares match on a perfect sample) and alpha:
# pe = p' W p. For these weights 1 - W is a distance of negative type, so pe
# is convex in p. Relax each group to any split of its mass over its
# ratings: the relaxed set's vertices send each group whole to one rating,
# which real corrections do, and a convex function peaks at a vertex.
quadratic_chance_max <- function(base, groups, total, weight_mat, max_vertices) {
  # Merging the two lightest groups over the union of their ratings only
  # adds corrections, so the maximum stays an upper bound.
  while (length(groups) > 1L &&
         prod(lengths(lapply(groups, `[[`, "values"))) > max_vertices) {
    lightest <- order(vapply(groups, `[[`, numeric(1), "mass"))[1:2]
    a <- groups[[lightest[[1]]]]
    b <- groups[[lightest[[2]]]]
    groups <- c(groups[-lightest], list(list(
      values = sort(union(a$values, b$values)),
      mass = a$mass + b$mass,
      min_mass = min(a$min_mass, b$min_mass)
    )))
  }
  q <- length(base)
  choice <- as.matrix(expand.grid(lapply(groups, `[[`, "values")))
  totals <- matrix(base, nrow(choice), q, byrow = TRUE)
  for (g in seq_along(groups)) {
    cell <- cbind(seq_len(nrow(choice)), choice[, g])
    totals[cell] <- totals[cell] + groups[[g]]$mass
  }
  totals <- totals[rowSums(totals > 0) >= 2L, , drop = FALSE]
  shares <- totals / total
  best <- if (nrow(shares) > 0L) {
    max(rowSums((shares %*% weight_mat) * shares))
  } else {
    NA_real_
  }
  # A vertex with every item in category k is not a valid correction. When
  # the others cannot reach it either (only k among the agreeing items, and
  # k among every group's ratings), the valid corrections nearest it move
  # the lightest item of one group to another of its ratings; convexity
  # along that group's split puts the maximum there or at a valid vertex.
  # Each has two categories, so its pe is found directly: there can be
  # q (q - 1) of them.
  for (k in seq_len(q)) {
    reachable <- vapply(groups, function(g) k %in% g$values, logical(1))
    if (any(base[-k] > 0) || !all(reachable)) next
    for (g in groups) {
      if (g$min_mass >= total) next
      j <- setdiff(g$values, k)
      a <- (total - g$min_mass) / total
      b <- g$min_mass / total
      near <- a^2 * weight_mat[k, k] + b^2 * weight_mat[cbind(j, j)] +
        2 * a * b * weight_mat[k, j]
      best <- max(best, near, na.rm = TRUE)
    }
  }
  best
}

# Gwet's AC1 and AC2: pe = sum(W_S) / (q_S (q_S - 1)) * (1 - sum(p^2)) over
# the chance categories S, which a correction can shrink by dropping a
# category only disagreeing items used. For each possible S the concave part
# is bounded from above by Frank-Wolfe; letting a category of S go empty
# only adds candidates.
gwet_chance_max <- function(base, groups, total, weight_mat,
                            max_subsets = 256L) {
  fixed <- which(base > 0)
  optional <- setdiff(unlist(lapply(groups, `[[`, "values")), fixed)
  mass <- vapply(groups, `[[`, numeric(1), "mass")
  if (2^length(optional) > max_subsets) {
    return(gwet_chance_ceiling(base, groups, mass, total, weight_mat, fixed,
                               optional))
  }
  best <- NA_real_
  for (bits in seq_len(2^length(optional)) - 1L) {
    extra <- optional[bitwAnd(bits, 2^(seq_along(optional) - 1L)) > 0]
    s <- sort(c(fixed, extra))
    allowed <- lapply(groups, function(g) intersect(g$values, s))
    if (length(s) < 2L || any(lengths(allowed) == 0L)) next
    scale <- sum(weight_mat[s, s]) / (length(s) * (length(s) - 1L))
    spread <- balanced_share_max(base, allowed, mass, total)
    best <- max(best, scale * spread, na.rm = TRUE)
  }
  best
}

# The subsets of categories only disagreeing items used grow exponentially
# (a 0-100 score scale can have dozens), so past a few the two factors are
# bounded apart. The share term is largest with every category allowed and
# is at most 1 - 1/m over m categories. A set of m categories holds every
# fixed one and m - |fixed| optional ones, so its weight term is at most
# (m + rows) / (m (m - 1)) with rows bounding its off-diagonal weights row
# by row: a fixed row's weights to the other fixed categories and its
# m - |fixed| largest to optional ones, and the m - |fixed| largest of the
# optional rows' weights to every fixed category and their m - |fixed| - 1
# largest to other optional ones. With no optional category it is exact.
gwet_chance_ceiling <- function(base, groups, mass, total, weight_mat, fixed,
                                optional) {
  spread <- balanced_share_max(
    base, lapply(groups, `[[`, "values"), mass, total
  )
  off <- weight_mat
  diag(off) <- 0
  top_sum <- function(w, k) sum(sort(w, decreasing = TRUE)[seq_len(k)])
  fixed_within <- sum(off[fixed, fixed])
  best <- NA_real_
  for (m in max(2L, length(fixed)):(length(fixed) + length(optional))) {
    extra <- m - length(fixed)
    rows <- fixed_within
    if (extra > 0L) {
      fixed_rows <- vapply(fixed, function(f) {
        top_sum(off[f, optional], extra)
      }, numeric(1))
      optional_rows <- vapply(optional, function(o) {
        sum(off[o, fixed]) + top_sum(off[o, setdiff(optional, o)], extra - 1L)
      }, numeric(1))
      rows <- rows + sum(fixed_rows) + top_sum(optional_rows, extra)
    }
    scale <- (m + rows) / (m * (m - 1))
    best <- max(best, scale * min(spread, 1 - 1 / m), na.rm = TRUE)
  }
  best
}

# Upper bound on the largest 1 - sum(((base + y) / total)^2), y the groups'
# masses split over their allowed categories. The objective is concave, so
# at every iterate its value plus the Frank-Wolfe gap bounds the maximum.
balanced_share_max <- function(base, allowed, mass, total, tol = 1e-12,
                               max_iter = 500L) {
  q <- length(base)
  place <- function(target) {
    y <- numeric(q)
    for (g in seq_along(allowed)) y[target[[g]]] <- y[target[[g]]] + mass[[g]]
    y
  }
  y <- numeric(q)
  for (g in seq_along(allowed)) {
    y[allowed[[g]]] <- y[allowed[[g]]] + mass[[g]] / length(allowed[[g]])
  }
  upper <- Inf
  for (iter in seq_len(max_iter)) {
    current <- base + y
    # The linearisation is largest when each group joins its emptiest
    # allowed category.
    vertex <- place(lapply(allowed, function(v) v[which.min(current[v])]))
    direction <- vertex - y
    gap <- -2 * sum(current * direction) / total^2
    upper <- min(upper, 1 - sum(current^2) / total^2 + gap)
    if (gap <= tol) break
    step <- -sum(current * direction) / sum(direction^2)
    y <- y + min(1, max(0, step)) * direction
  }
  upper
}

# A coefficient is `(pa - pe) / (1 - pe)`; holding the sample's chance term
# `pe` fixed maps a bound on observed agreement `pa` onto its scale.
chance_mapped <- function(agreement, chance) {
  if (!is.finite(agreement) || length(chance) != 1L || !is.finite(chance) ||
      chance >= 1) {
    return(NA_real_)
  }
  (agreement - chance) / (1 - chance)
}

as_ratings <- function(x, rater_cols = NULL, call = rlang::caller_env()) {
  if (inherits(x, "yourhonor_calibration")) return(x$ratings)
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  check_unique_column_names(x, call = call)
  if (is.null(rater_cols)) {
    rater_cols <- infer_rater_cols(x)
  } else {
    rater_cols <- check_column_names(
      rater_cols, "rater_cols", minimum = 2L, call = call
    )
    check_columns_present(x, rater_cols, "rater_cols", call = call)
  }
  x <- x[, rater_cols, drop = FALSE]
  if (ncol(x) < 2L) {
    cli::cli_abort(
      paste0(
        "Ratings need at least two columns ",
        "(e.g. {.field human} and {.field judge})."
      ),
      call = call
    )
  }
  if (nrow(x) < 2L) {
    cli::cli_abort(
      "Need at least 2 rated items to estimate agreement.",
      call = call
    )
  }
  x
}

# Inference is convenient for the common two-column case but easy to get wrong
# on wider frames, so any choice beyond the documented default is reported.
infer_rater_cols <- function(x) {
  default_pair <- c("human", "judge")
  if (all(default_pair %in% names(x))) {
    ignored <- setdiff(names(x), default_pair)
    if (length(ignored) > 0L) {
      inform_inferred_cols(
        c(
          "Using {.field human} and {.field judge} as rater columns.",
          "Ignoring column{?s}: {.field {ignored}}."
        ),
        "rater_cols"
      )
    }
    return(default_pair)
  }
  inform_inferred_cols(
    "Treating all {ncol(x)} column{?s} as raters: {.field {names(x)}}.",
    "rater_cols"
  )
  names(x)
}

# Classed so callers can `suppressMessages()` selectively; `.envir` lets the
# glue expressions see the caller's variables.
inform_inferred_cols <- function(message, arg, .envir = parent.frame()) {
  hint <- cli::format_inline("Set {.arg {arg}} to choose explicitly.")
  cli::cli_inform(
    c(
      stats::setNames(message, rep("i", length(message))),
      " " = hint
    ),
    class = "yourhonor_inferred_columns",
    .envir = .envir
  )
}

coeff_point <- function(ratings,
                        coefficient,
                        weights = "unweighted",
                        label_levels = NULL,
                        chance_levels = label_levels) {
  coeff_fit(ratings, coefficient, weights, label_levels, chance_levels)$value
}

# The estimate, its chance term `pe` and observed agreement `pa` from one
# irrCAC call. Weights are built over the full declared scale, so an ordinal
# distance never depends on which levels were observed, then restricted to
# `chance_levels`, the categories the chance term counts.
coeff_fit <- function(ratings,
                      coefficient,
                      weights = "unweighted",
                      label_levels = NULL,
                      chance_levels = label_levels) {
  if (coefficient == "cohen_kappa" && ncol(ratings) != 2L) {
    cli::cli_abort(
      "{.val cohen_kappa} is defined for exactly two raters; got {ncol(ratings)}."
    )
  }
  if (is.null(label_levels)) label_levels <- sort(observed_categories(ratings))
  if (is.null(chance_levels)) chance_levels <- label_levels
  coded <- encode_ratings(ratings, label_levels)
  categ <- match(chance_levels, label_levels)
  weight_mat <- agreement_weight_matrix(weights, length(label_levels))
  weight_mat <- weight_mat[categ, categ, drop = FALSE]
  fn <- switch(
    coefficient,
    cohen_kappa = irrCAC::conger.kappa.raw,
    krippendorff_alpha = irrCAC::krippen.alpha.raw,
    gwet_ac1 = ,
    gwet_ac2 = irrCAC::gwet.ac1.raw,
    cli::cli_abort("Unknown coefficient {.val {coefficient}}.")
  )
  est <- fn(coded, weights = weight_mat, categ.labels = categ)$est
  list(
    value = as.numeric(est$coeff.val),
    chance = as.numeric(est$pe),
    agreement = as.numeric(est$pa)
  )
}

# irrCAC's own weight schemes, indexed by declared position.
agreement_weight_matrix <- function(weights, q) {
  categ <- seq_len(q)
  switch(
    weights,
    unweighted = irrCAC::identity.weights(categ),
    linear = irrCAC::linear.weights(categ),
    quadratic = irrCAC::quadratic.weights(categ),
    ordinal = irrCAC::ordinal.weights(categ),
    cli::cli_abort("Unknown weights {.val {weights}}.")
  )
}

# irrCAC warns on some degenerate resamples and errors on others. A warning
# still comes with a value, which is kept when finite; an error has none and
# becomes NA, which the caller reports as "undefined".
safe_coeff_fit <- function(ratings,
                           coefficient,
                           weights,
                           label_levels,
                           chance_levels = label_levels) {
  fit <- tryCatch(
    suppressWarnings(
      coeff_fit(ratings, coefficient, weights, label_levels, chance_levels)
    ),
    error = function(e) {
      list(value = NA_real_, chance = NA_real_, agreement = NA_real_)
    }
  )
  if (length(fit$value) != 1L || !is.finite(fit$value)) {
    fit$value <- NA_real_
  }
  fit
}

safe_coeff_point <- function(ratings,
                             coefficient,
                             weights,
                             label_levels,
                             chance_levels = label_levels) {
  safe_coeff_fit(
    ratings, coefficient, weights, label_levels, chance_levels
  )$value
}

resolve_gwet_coefficient <- function(coefficients,
                                     weights,
                                     auto = FALSE,
                                     call = rlang::caller_env()) {
  if (weights == "unweighted") {
    if ("gwet_ac2" %in% coefficients) {
      cli::cli_abort(
        c(
          "{.val gwet_ac2} requires non-default agreement weights.",
          i = paste0(
            "Set {.arg weights}, or request {.val gwet_ac1} or leave ",
            "{.arg coefficients} as {.code NULL} for the unweighted ",
            "coefficient."
          )
        ),
        call = call
      )
    }
    return(coefficients)
  }
  if ("gwet_ac1" %in% coefficients) {
    if (!auto) {
      cli::cli_abort(
        paste0(
          "Weighted Gwet agreement is AC2, not AC1. Request {.val gwet_ac2} ",
          "or leave {.arg coefficients} as {.code NULL} to select it ",
          "automatically under {.arg weights}."
        ),
        call = call
      )
    }
    coefficients[coefficients == "gwet_ac1"] <- "gwet_ac2"
  }
  coefficients
}

# Landis & Koch (1977) proposed these bands for kappa. They are applied to
# every coefficient here as a reading aid only; see `?agreement`.
interpret_agreement <- function(value) {
  if (length(value) != 1L || is.na(value)) return(NA_character_)
  # "poor" is strictly below 0; from 0 the bands are 0-0.20, 0.21-0.40, and
  # so on, each closed at its upper edge, so 0 itself is "slight".
  if (value < 0) return("poor")
  breaks <- c(-Inf, 0.2, 0.4, 0.6, 0.8, Inf)
  labels <- c("slight", "fair", "moderate", "substantial", "almost perfect")
  as.character(cut(value, breaks = breaks, labels = labels, right = TRUE))
}
