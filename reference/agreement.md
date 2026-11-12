# Chance-corrected agreement between a judge and humans

Reports how closely a set of judge labels agrees with human labels.
Confidence intervals use a nonparametric item bootstrap. The categories
the sample observed are held fixed across bootstrap draws so rare
categories do not silently disappear from the estimand.

## Usage

``` r
agreement(
  x,
  coefficients = NULL,
  conf_level = 0.95,
  n_boot = 2000,
  seed = NULL,
  label_levels = NULL,
  weights = c("unweighted", "linear", "quadratic", "ordinal"),
  rater_cols = NULL
)
```

## Arguments

- x:

  Either a `yourhonor_calibration` object or a data frame / tibble of
  ratings with one column per rater (see `rater_cols`). Rows with fewer
  than two observed ratings do not contribute, while partially observed
  multi-rater rows remain available to coefficients that support them.

  For a calibration object every argument left at its default is
  inherited from the calibration (`coefficients`, `conf_level`,
  `n_boot`, `seed`, `label_levels`, and `weights`), so
  `agreement(cal, n_boot = 50)` reuses the stored seed and confidence
  level. When nothing is overridden the stored agreement table is
  returned unchanged.

- coefficients:

  Character vector containing any of `"cohen_kappa"`,
  `"krippendorff_alpha"`, `"gwet_ac1"`, and `"gwet_ac2"`. `NULL` (the
  default) selects `"cohen_kappa"`, `"krippendorff_alpha"`, and
  `"gwet_ac1"`, replacing `"gwet_ac1"` with its weighted counterpart
  `"gwet_ac2"` when `weights` is not `"unweighted"`. Explicitly
  requesting `"gwet_ac1"` together with weights is an error rather than
  a silent substitution.

- conf_level:

  Confidence level for the bootstrap interval.

- n_boot:

  Positive number of bootstrap resamples. One set of item resamples is
  shared by every coefficient, so intervals from one call are computed
  on identical draws.

- seed:

  Optional integer seed. The caller's random state is restored.

- label_levels:

  Complete label universe in its intended order. Supplying this is
  strongly recommended for ordinal scales and rare classes. Labels are
  matched exactly: case and surrounding whitespace are significant, and
  empty or whitespace-only labels are rejected. Use `NA` for a missing
  or failed judgement.

- weights:

  Agreement weights. `"unweighted"` treats labels as nominal;
  `"linear"`, `"quadratic"`, and `"ordinal"` use the order in
  `label_levels` and are Gwet's weight schemes as implemented by irrCAC
  (`linear.weights()`, `quadratic.weights()`, and `ordinal.weights()`).
  Every weighted coefficient, including `"krippendorff_alpha"`, uses
  these schemes; `weights = "ordinal"` is Gwet's combinatorial ordinal
  weighting and is not Krippendorff's ordinal alpha, which weights by
  cumulative category frequencies.

- rater_cols:

  Optional character vector naming the rater columns of a ratings data
  frame. When `NULL`, columns named `human` and `judge` are used if both
  exist; otherwise every column is treated as a rater. A message (class
  `yourhonor_inferred_columns`) reports the choice whenever inference
  ignored columns or fell back to all columns; supply `rater_cols` to
  choose explicitly. Cannot be combined with a calibration object, which
  carries its own ratings.

## Value

A tibble with one row per coefficient and columns `coefficient`,
`estimate`, `conf_low`, `conf_high`, `se`, `n_items`, `conf_low_exact`,
`n_input`, `n_raters`, `n_missing_rows`, `weights`, `n_boot_effective`,
`boundary_degenerate`, `ci_reliable`, `status`, and `interpretation`.

`status` is `"ok"` when the estimate and interval were computed, or one
of `"insufficient_items"`, `"insufficient_variation"`,
`"insufficient_reference_variation"`, `"invalid_rater_count"`, and
`"undefined"`. `"insufficient_reference_variation"` applies to a
calibration object, whose human (consensus) reference must use at least
two categories: against a single-class reference every chance-corrected
coefficient is uninformative, and Gwet's AC1 in particular can read
"almost perfect" while kappa reads zero. The rule applies to the full
sample only: a bootstrap resample whose reference happens to hold one
category still contributes its coefficient.

`conf_low_exact` is a resampling-free lower bound: a finite-sample bound
on the coefficient's own observed agreement, one-sided at level
`(1 + conf_level) / 2` like the lower end of the bootstrap interval, and
mapped onto the coefficient's scale with the sample's chance term held
fixed. It stays honest where the percentile bootstrap is optimistic (at
small `n`, and on a perfect sample), and it is weak for kappa and alpha
on imbalanced labels, whose chance term is large. It never exceeds
`estimate`, is reported for every coefficient with `status = "ok"`, and
is `NA` otherwise. Policies assess the smaller of `conf_low` and
`conf_low_exact`; see
[`acceptance_policy()`](https://jsizzler.github.io/yourhonor/reference/acceptance_policy.md).
How the bound is built, its cap, how fast it tightens and its coverage
in simulation are in
[`vignette("exact-bound", package = "yourhonor")`](https://jsizzler.github.io/yourhonor/articles/exact-bound.md).

`ci_reliable` is `TRUE` only when at least 30 resamples, and at least
80% of the requested resamples, produced a finite statistic, and the
interval is not `boundary_degenerate`. With `n_boot` below 30 every
interval is therefore unreliable. A perfect finite sample produces a
zero-width nonparametric bootstrap, and that interval is flagged
unreliable rather than treated as proof of population-perfect agreement;
`conf_low_exact` is the usable lower bound there. The percentile
bootstrap is also optimistic at small `n` (around 30 items or fewer),
which is why policies also read `conf_low_exact`.

`interpretation` applies the Landis and Koch (1977) bands (`"poor"`
below 0, then `"slight"`, `"fair"`, `"moderate"`, `"substantial"`, and
`"almost perfect"` at 0.2 steps). Those bands were proposed for kappa;
applied to alpha, AC1, or AC2 they are a heuristic reading aid, not a
validated scale, and the same data can land in different bands under
different coefficients.

## Details

Chance agreement counts the categories some rater used, not every
declared level: Gwet's AC1 and AC2 divide by the number of categories,
so a declared level that no rater used would otherwise raise them on
identical data (Cohen's kappa and Krippendorff's alpha are unaffected by
unused levels). Unweighted, an unused declared level therefore changes
no coefficient. Weighted coefficients still take their weights from the
full declared scale, so an ordinal distance never depends on which
levels were observed. An unused level still changes the weights between
the observed levels, and every weighted chance term sums those weights:
with linear weights and levels 1 to 3 observed, declaring a fourth level
can move AC2's chance term from about 0.55 to 0.70 and so change AC2
itself. Kappa's and alpha's chance terms move too. For a level beyond
the observed ones each weight scheme only rescales the distances between
observed levels, so their observed agreement moves with the chance term
and their estimates do not change (their `conf_low_exact` does). A level
between observed ones can change those distances and the weighted
estimates with them.

Agreement is descriptive evidence, not a universal fitness gate. In
particular, a high aggregate coefficient can coexist with zero recall on
a rare, important class. Use
[`report()`](https://jsizzler.github.io/yourhonor/reference/report.md)
with an explicit
[`acceptance_policy()`](https://jsizzler.github.io/yourhonor/reference/acceptance_policy.md)
and inspect
[`confusion_metrics()`](https://jsizzler.github.io/yourhonor/reference/confusion_metrics.md)
before making a deployment decision.

## References

Gwet, K. L. (2014). *Handbook of Inter-Rater Reliability* (4th ed.).
Advanced Analytics, LLC.

Landis, J. R., & Koch, G. G. (1977). The measurement of observer
agreement for categorical data. *Biometrics*, 33(1), 159-174.

## Examples

``` r
ratings <- data.frame(
  human = c("C", "C", "P", "I", "C", "P", "I", "C"),
  judge = c("C", "C", "C", "I", "C", "P", "P", "C")
)
agreement(ratings, n_boot = 200, seed = 1,
          label_levels = c("I", "P", "C"), weights = "ordinal",
          rater_cols = c("human", "judge"))
#> # A tibble: 3 × 16
#>   coefficient   estimate conf_low conf_high conf_low_exact    se n_items n_input
#>   <chr>            <dbl>    <dbl>     <dbl>          <dbl> <dbl>   <int>   <int>
#> 1 cohen_kappa      0.758    0.352         1       -0.313   0.184       8       8
#> 2 krippendorff…    0.767    0.283         1       -0.341   0.194       8       8
#> 3 gwet_ac2         0.813    0.561         1       -0.00832 0.117       8       8
#> # ℹ 8 more variables: n_raters <int>, n_missing_rows <int>, weights <chr>,
#> #   n_boot_effective <int>, boundary_degenerate <lgl>, ci_reliable <lgl>,
#> #   status <chr>, interpretation <chr>
```
