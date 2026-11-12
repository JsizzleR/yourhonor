# Probe a pairwise judge for position bias

Judges every pair in original and swapped display order. Display-space
choices are mapped back to stable candidate identity before consistency
is measured, so a content-consistent judge remains consistent and an
always-first judge is exposed.

## Usage

``` r
probe_position(
  data,
  judge,
  candidate_a_col,
  candidate_b_col,
  context_cols = NULL,
  choice_levels = c("A", "B"),
  tie_label = NULL,
  repeats = 2,
  seed = NULL,
  conf_level = 0.95,
  alpha = 0.05,
  audit_id = NULL
)
```

## Arguments

- data:

  A data frame with separate character candidate columns (a list,
  factor, numeric or matrix column is an error). Missing candidates are
  an error, as for
  [`probe_bias()`](https://jsizzler.github.io/yourhonor/reference/probe_bias.md):
  a pair with an `NA` candidate would otherwise be graded as a real
  comparison and hide abstention from the missingness gate.

- judge:

  A judge function receiving an opaque, unique `.item_id`, neutral
  `.candidate_1` and `.candidate_2` display slots, and any
  `context_cols`. Source column names, condition, repeat, and
  canonical-pair identities are retained internally and never exposed.
  The judge returns display-space choices such as `"A"`, `"B"`, or the
  optional tie label.

- candidate_a_col, candidate_b_col:

  Column names containing canonical candidates A and B.

- context_cols:

  Optional context columns passed through unchanged.

- choice_levels:

  Two display labels for the first and second candidate.

- tie_label:

  Optional tie label.

- repeats:

  Number of original/swapped replications per item. Each condition's
  judgment is the mode across repeats, so with an even number a split
  vote ties and leaves the item unscored. An odd number avoids two-way
  ties when the judge returns no tie label and no `NA`; an acceptance
  policy counts unscored items against the probe in its upper bounds, so
  they can keep it from passing (see
  [`acceptance_policy()`](https://jsizzler.github.io/yourhonor/reference/acceptance_policy.md)'s
  `max_probe_flip_rate`). Taking the mode also attenuates a bias that
  appears on a minority of calls: a judge that, independently on each
  call, takes the first slot with probability `q < 0.5` and otherwise
  picks the better candidate shows a modal flip rate of `3q^2 - 2q^3`
  with three repeats, against `q` with one.

- seed:

  Optional R random seed used to randomize condition order.

- conf_level:

  Confidence level for exact binomial intervals.

- alpha:

  Significance level for first-position preference.

- audit_id:

  Optional evidence-bundle identifier. Use the same value as the related
  calibration and stability run.

## Value

A one-row `yourhonor_bias` tibble (see *Result columns*) with the
attributes `audit_id`, `evidence`, and `item_evidence`. Both evidence
attributes are lists with one `"position"` element: `evidence` holds one
row per judgment (condition, repeat, display and canonical choice);
`item_evidence` holds one row per item (modal choice per condition,
`flipped`, `display_first_rate`, `position_direction`).

## Result columns

[`probe_bias()`](https://jsizzler.github.io/yourhonor/reference/probe_bias.md),
`probe_position()`, and
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
pairs <- tibble::tibble(
  question = paste("question", 1:20),
  answer_a = paste("A complete, sourced answer to question", 1:20),
  answer_b = paste("A vague reply to question", 1:20)
)
# Picks the complete answer wherever it is shown: no position bias.
content_judge <- function(d) {
  ifelse(grepl("complete", d$.candidate_1), "A", "B")
}
fair <- probe_position(pairs, content_judge, "answer_a", "answer_b",
                       context_cols = "question", seed = 1)
fair[c("flip_rate", "direction", "position_preference", "verdict")]
#> 
#> ── yourhonor bias probes ──
#> 
#> # A tibble: 1 × 4
#>   flip_rate direction position_preference verdict  
#>       <dbl> <chr>                   <dbl> <chr>    
#> 1         0 balanced                   NA no_signal

# Always picks the first slot, which swapping the display order exposes.
first_judge <- function(d) rep("A", nrow(d))
biased <- probe_position(pairs, first_judge, "answer_a", "answer_b",
                         seed = 1)
biased[c("flip_rate", "direction", "position_preference", "verdict")]
#> ── yourhonor bias probes ──
#> 
#> # A tibble: 1 × 4
#>   flip_rate direction    position_preference verdict
#>       <dbl> <chr>                      <dbl> <chr>  
#> 1         1 favors_first                   1 BIAS   
```
