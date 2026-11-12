# Combine bias-probe evidence

Combines pointwise and position results while preserving and validating
their shared `audit_id`. Use this when an acceptance policy requires
probes from both
[`probe_bias()`](https://jsizzler.github.io/yourhonor/reference/probe_bias.md)
and
[`probe_position()`](https://jsizzler.github.io/yourhonor/reference/probe_position.md).

## Usage

``` r
combine_bias(..., p_adjust = c("holm", "none", "BH", "bonferroni"))
```

## Arguments

- ...:

  Two or more `yourhonor_bias` objects.

- p_adjust:

  Multiple-comparison correction applied across all combined probes.
  This replaces the within-call adjusted p-values, and verdicts are
  recomputed from the new `p_value_adj` with each row's own `alpha` and
  `flip_threshold`.

## Value

One `yourhonor_bias` object with one row per unique probe. The
`evidence` and `item_evidence` attributes are the constituents' lists,
concatenated and still named by probe. Only entries for probes present
in a constituent's rows are carried over, so evidence left behind by
row-subsetting a result cannot shadow a newer run of the same probe.

## Result columns

[`probe_bias()`](https://jsizzler.github.io/yourhonor/reference/probe_bias.md),
[`probe_position()`](https://jsizzler.github.io/yourhonor/reference/probe_position.md),
and `combine_bias()` return one row per probe with the same columns.
"Items" are rows of `data`; "judgments" are individual judge outputs.
Columns that apply only to the position probe are `NA` for pointwise
probes.

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
pointwise <- probe_bias(
  tibble::tibble(input = paste("answer", 1:20)), length_judge,
  probes = c("verbosity", "self_preference"), audit_id = "run-1", seed = 1
)

pairs <- tibble::tibble(
  answer_a = paste("A complete answer", 1:20),
  answer_b = paste("A vague reply", 1:20)
)
first_judge <- function(d) rep("A", nrow(d))
position <- probe_position(pairs, first_judge, "answer_a", "answer_b",
                           audit_id = "run-1", seed = 1)

# One family of three probes, Holm-adjusted together.
combined <- combine_bias(pointwise, position)
combined[c("probe", "direction", "p_value_adj", "verdict")]
#> 
#> ── yourhonor bias probes ──
#> 
#> # A tibble: 3 × 4
#>   probe           direction        p_value_adj verdict  
#>   <chr>           <chr>                  <dbl> <chr>    
#> 1 verbosity       favors_perturbed  0.00000572 BIAS     
#> 2 self_preference none              1          no_signal
#> 3 position        favors_first      0.00000572 BIAS     
```
