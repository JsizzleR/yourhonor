# Test-retest stability of a judge

Re-grades the same items and reports chance-corrected agreement plus
mean pairwise disagreement. `any_flip_rate_at_n_runs` is retained as a
descriptive quantity but must not be compared across different `n_runs`,
because it rises mechanically as more opportunities to flip are
observed.

## Usage

``` r
test_retest(
  data,
  judge,
  input_cols = NULL,
  id_col = NULL,
  n_runs = 5,
  seed = NULL,
  label_levels = NULL,
  weights = c("unweighted", "linear", "quadratic", "ordinal"),
  conf_level = 0.95,
  n_boot = 1000,
  audit_id = NULL
)
```

## Arguments

- data:

  A data frame of items to grade.

- judge:

  A judge function `(data) -> labels`.

- input_cols:

  Columns visible to `judge`. Required so labels and other audit-only
  fields are not passed through accidentally.

- id_col:

  Optional unique source identifier. It is validated but is not exposed
  to the judge unless explicitly included in `input_cols`.

- n_runs:

  Number of repeated gradings; must be at least two.

- seed:

  Optional R random seed. This does not force deterministic behavior
  from a remote model provider.

- label_levels:

  Optional complete label universe. Unweighted, a declared level no run
  used changes no coefficient. Weighted, the weights come from the full
  declared scale, so an unused level can still change AC2; see
  [`agreement()`](https://jsizzler.github.io/yourhonor/reference/agreement.md).

- weights:

  Agreement weights passed to Gwet's coefficient; see
  [`agreement()`](https://jsizzler.github.io/yourhonor/reference/agreement.md)
  for the weight schemes.

- conf_level:

  Confidence level for every interval.

- n_boot:

  Number of item-bootstrap resamples.

- audit_id:

  Optional evidence-bundle identifier. Use the same value as the related
  calibration and bias probes.

## Value

A `yourhonor_stability` object with raw runs, per-item evidence,
missingness, pairwise disagreement, Gwet agreement, and intervals.
`stability_status` explains why chance-corrected agreement is
unavailable; in particular, one observed category is
`insufficient_variation`, not perfect AC1/AC2, and `stability_conf_low`
and `stability_conf_high` are then `NA`. The pairwise-disagreement
interval remains available.

The pairwise-disagreement interval is a hybrid: the percentile item
bootstrap widened to include a Wilson score interval for the mean
per-item disagreement over `n_items_scored` items. The Wilson interval
keeps a zero-disagreement sample from producing a zero-width interval
and typically sets both bounds once `n_runs > 2`, so this interval is
conservative relative to the bootstrap alone and widens only, never
narrows. The chance-corrected interval is the plain item bootstrap.

Missing judgments are left out of the observed statistics, so a judge
that abstains where it would have been unstable looks stable there. The
worst-case fields bound that: `pairwise_disagreement_worst_case` is the
mean over all `n_items` items of a per-item disagreement in which every
pair involving a missing judgment disagrees (an item with fewer than two
judgments counts as 1; `per_item$pairwise_disagreement_worst_case` holds
the per-item values), and `pairwise_disagreement_worst_case_high` is its
upper bound, computed like the observed one on the same bootstrap draws
and never below `pairwise_disagreement_conf_high`. With no missing
judgment it equals `pairwise_disagreement_conf_high`, and it is `NA`
when that is (no item has two judgments). This is the upper bound
[`acceptance_policy()`](https://jsizzler.github.io/yourhonor/reference/acceptance_policy.md)
assesses. A numeric `NaN` judgment is missing, not a label.

`any_flip_rate_at_n_runs` is over the `n_items_complete` items with
every run observed (`NA` when there are none), since an incomplete item
had fewer chances to flip; under selective abstention it describes
complete items only. `any_flip_rate_worst_case` counts every item with
an observed flip or a missing judgment as flipped, over all `n_items`.

`runs` holds labels in source-row order. Because every run is graded
under a fresh shuffled key, `judge_item_ids` is an integer matrix
parallel to `runs` whose entry `[i, r]` is the `.item_id` the judge saw
for source row `i` in run `r`; join judge-side logs on that key, never
on the row number.

## Examples

``` r
noisy_judge <- function(d) sample(c("C", "P"), nrow(d), replace = TRUE)
data <- tibble::tibble(input = paste("answer", 1:10))
test_retest(data, noisy_judge, input_cols = "input",
            n_runs = 4, n_boot = 100, seed = 1)
#> 
#> ── yourhonor test-retest stability ──
#> 
#> 4 runs - pairwise disagreement 0.45 [0.20, 0.73].
#> gwet_ac1: 0.12 [-0.15, 0.52].
#> Any-flip rate at exactly 4 runs: 0.80; missing judgments: 0.00.
```
