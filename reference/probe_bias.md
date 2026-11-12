# Probe a pointwise judge for paired bias

Grades each item under a control and a perturbed treatment condition in
one randomized batch and measures directional label movement. The
verbosity probe deliberately lengthens the treatment; the
self-preference and bandwagon probes change only a framing prefix.
Position bias is intentionally handled by
[`probe_position()`](https://jsizzler.github.io/yourhonor/reference/probe_position.md),
which uses structured candidates and stable identities.

## Usage

``` r
probe_bias(
  data,
  judge,
  input_cols = "input",
  id_col = NULL,
  probes = c("verbosity", "self_preference", "bandwagon"),
  ordered_levels = c("I", "P", "C"),
  n = NULL,
  seed = NULL,
  flip_threshold = 0.1,
  alpha = 0.05,
  p_adjust = c("holm", "none", "BH", "bonferroni"),
  conf_level = 0.95,
  audit_id = NULL
)
```

## Arguments

- data:

  A data frame with a character `input` column. Missing inputs are an
  error: a pasted `NA` would otherwise be graded as a real answer and
  hide abstention from the missingness gate.

- judge:

  A judge function.

- input_cols:

  Columns visible to `judge`. The default exposes only `input`; all
  other columns are redacted.

- id_col:

  Optional unique source identifier. It is validated but is not exposed
  to the judge unless explicitly included in `input_cols`.

- probes:

  Any of `"verbosity"`, `"self_preference"`, and `"bandwagon"`.

- ordered_levels:

  Label scale from worst to best.

- n:

  Optional positive number of sampled items.

- seed:

  Optional R random seed.

- flip_threshold:

  Practical-significance floor for a `"BIAS"` verdict: the net
  `directional_rate` (see *Result columns*) must exceed it in addition
  to `p_value_adj < alpha`. A large but balanced `flip_rate` is reported
  yet never flagged.

- alpha:

  Significance level.

- p_adjust:

  Multiple-comparison correction applied across the requested probes.
  Probes with no label movement enter the family with `p_value = 1`;
  only `"insufficient_data"` probes are excluded.

- conf_level:

  Confidence level for the exact flip-rate interval.

- audit_id:

  Optional evidence-bundle identifier. Use the same value as the related
  calibration and stability run.

## Value

A `yourhonor_bias` tibble with one row per probe (see *Result columns*)
and the attributes `audit_id`, `evidence`, and `item_evidence`. The two
evidence attributes are lists named by probe; pointwise probes
contribute no entries. `"no_signal"` means the probe did not detect
directional bias; it is not proof that the judge is unbiased.

## Result columns

`probe_bias()`,
[`probe_position()`](https://jsizzler.github.io/yourhonor/reference/probe_position.md),
and
[`combine_bias()`](https://jsizzler.github.io/yourhonor/reference/combine_bias.md)
return one row per probe with the same columns. "Items" are rows of
`data`; "judgments" are individual judge outputs. Columns that apply
only to the position probe are `NA` for pointwise probes.

- `probe`: probe name.

- `flip_rate`: any-direction change rate, `n_discordant / n_scored`,
  with its exact (Clopper-Pearson) interval `conf_low`/`conf_high` at
  `conf_level`. `NA` when nothing was scored.

- `direction`: pointwise `"favors_perturbed"`, `"favors_original"`,
  `"balanced"`, or `"none"` (no movement); position `"favors_first"`,
  `"favors_second"`, or `"balanced"`.

- `directional_rate`: net movement over scored items. Pointwise:
  `|improved - worsened| / n_scored`. Position:
  `|first - second| / n_scored`, where `first`/`second` count items
  whose canonical choice moved with the display order towards one slot.
  `NA` when nothing was scored.
  `directional_conf_low`/`directional_conf_high` is a Bonferroni
  combination of exact marginal intervals for that difference
  (conservative). Its upper bound is capped at `conf_high`, because the
  net rate can never exceed the any-direction flip rate over the same
  denominator.

- `position_preference`, `position_conf_low`, `position_conf_high`:
  position only. Share of directional items favouring the first display
  slot (`statistic / n_directional_items`) with its exact interval.

- `display_first_rate`: position only. Mean per-item share of decided
  judgments that chose the first display slot.

- `statistic`: sign-test successes (`improved` pointwise; items
  favouring the first slot for position).

- `n_discordant`: scored items whose label (pointwise) or canonical
  choice (position) changed between conditions.

- `n_scored`: items with a decided judgment in both conditions. For the
  position probe a condition's judgment is the mode across `repeats`, so
  a within-condition tie leaves the item unscored.

- `n_directional_items`: items entering the sign test. Equals
  `n_discordant` for both probes: a position item is directional only
  when it was decided in both display orders and its canonical choice
  moved with the order (a tie commits to neither slot).

- `n_input`: items supplied, after any sampling.

- `n_unscored`, `unscored_rate`: `n_input - n_scored` and its share of
  `n_input`. Includes position items lost to within-condition ties, so
  it can exceed `n_items_missing`.

- `n_unscored_tied`: position items lost to a within-condition tie that
  no answer could break: a display order judged in every repeat whose
  modal choice is tied, whatever the other order's missing judgments.
  Always 0 for pointwise probes, where an item is unscored only through
  a missing judgment.

- `n_items_missing`: items with at least one `NA` judgment.

- `n_judgments`, `n_judgments_missing`, `missing_rate`: individual judge
  outputs, how many were `NA`, and their share. `missing_rate` is gated
  directly by
  [`acceptance_policy()`](https://jsizzler.github.io/yourhonor/reference/acceptance_policy.md)'s
  `max_probe_missing_rate`; unscored items also enter the policy's
  flip-rate and directional bounds over `n_input`, against the judge in
  the upper bounds and in its favour in the lower bounds (which leave
  out the `n_unscored_tied` items, so their denominator is
  `n_input - n_unscored_tied`), so the intervals a policy assesses can
  be wider than this row's `conf_low`-`conf_high` and
  `directional_conf_low`-`directional_conf_high`, which describe the
  scored items.

- `n_comparisons`: condition pairs judged (`n_input` pointwise;
  `n_input * repeats` position).

- `p_value`: exact sign-test p-value over `n_directional_items`; `1`
  when no item moved, `NA` only when `status` is `"insufficient_data"`.

- `p_value_adj`: `p_value` corrected with `p_adjust_method` across the
  rows of the object; `NA` p-values are excluded from the family.

- `verdict`: `"BIAS"` when `p_value_adj < alpha` and `directional_rate`
  exceeds `flip_threshold` (the position probe has no effect floor,
  shown as `flip_threshold = NA`); `"no_signal"` otherwise;
  `"INCONCLUSIVE"` when nothing was scored.

- `status`: `"ok"` or `"insufficient_data"` (`n_scored == 0`).

- `conf_level`, `alpha`, `flip_threshold`, `p_adjust_method`: the
  settings in force for that row.

## Examples

``` r
length_judge <- function(d) ifelse(nchar(d$input) > 40, "C", "P")
data <- tibble::tibble(input = paste("answer", 1:20))
probe_bias(data, length_judge, probes = "verbosity", seed = 1)
#> 
#> ── yourhonor bias probes ──
#> 
#> # A tibble: 1 × 33
#>   probe     flip_rate conf_low conf_high direction        directional_rate
#>   <chr>         <dbl>    <dbl>     <dbl> <chr>                       <dbl>
#> 1 verbosity         1    0.832         1 favors_perturbed                1
#> # ℹ 27 more variables: directional_conf_low <dbl>, directional_conf_high <dbl>,
#> #   position_preference <dbl>, display_first_rate <dbl>,
#> #   position_conf_low <dbl>, position_conf_high <dbl>, statistic <int>,
#> #   n_discordant <int>, n_scored <int>, n_directional_items <int>,
#> #   n_input <int>, n_unscored <int>, unscored_rate <dbl>,
#> #   n_unscored_tied <int>, n_items_missing <int>, n_judgments <int>,
#> #   n_judgments_missing <int>, missing_rate <dbl>, n_comparisons <int>, …
```
