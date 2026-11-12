# Declare a judge acceptance policy

A policy turns descriptive calibration evidence into a decision.
Thresholds are intentionally supplied by the caller because acceptable
error depends on the task and the cost of each class of mistake.

## Usage

``` r
acceptance_policy(
  min_agreement_lower,
  min_n,
  min_class_n,
  min_recall,
  max_missing_rate,
  max_pairwise_disagreement,
  min_stability_n = NULL,
  min_stability_runs = NULL,
  max_stability_missing_rate = NULL,
  required_probes = character(0),
  min_probe_n = NULL,
  max_probe_flip_rate = NULL,
  max_probe_directional_rate = NULL,
  max_probe_missing_rate = NULL,
  min_precision = NULL,
  min_human_reliability_lower = NULL,
  conf_level = 0.95,
  coefficient = NULL,
  max_abstention_rate = NULL
)
```

## Arguments

- min_agreement_lower:

  Minimum acceptable lower confidence bound for the selected agreement
  coefficient. The bound assessed is the smaller of the bootstrap
  `conf_low` and the exact `conf_low_exact` reported by
  [`agreement()`](https://jsizzler.github.io/yourhonor/reference/agreement.md).
  On a perfect sample the bootstrap interval is a zero-width point at 1
  and only `conf_low_exact` is used, so a perfect sample is assessed on
  the same footing as a near-perfect one rather than reading
  `INDETERMINATE` where a single error could `PASS`. Comparing a
  two-sided interval's lower bound with the threshold is a one-sided
  test at level `(1 + conf_level) / 2`, and taking the smaller of two
  lower bounds makes it more conservative than that.

- min_n:

  Minimum usable judge-human pairs.

- min_class_n:

  Minimum human-reference support for every class.

- min_recall:

  Minimum lower confidence bound for per-class recall. A scalar applies
  to all classes; a named vector applies only to named classes.

- max_missing_rate:

  Maximum fraction of calibration rows without a usable judge-consensus
  pair.

- max_pairwise_disagreement:

  Maximum upper confidence bound for mean pairwise stability
  disagreement. The interval assessed reads missing judgments both ways,
  over all `n_items` items (see
  [`test_retest()`](https://jsizzler.github.io/yourhonor/reference/test_retest.md)):
  its upper bound is the stability's
  `pairwise_disagreement_worst_case_high`, which counts every pair
  involving a missing judgment as a disagreement, and its lower bound is
  `pairwise_disagreement_best_case_low`, which gives each missing
  judgment the label that least disagrees with its item's other runs.
  Missing judgments can therefore turn this check's `PASS` or `FAIL`
  into `INDETERMINATE` but never cause either: a `FAIL` holds under the
  most favourable labels for them, so abstaining on runs that agree
  cannot fail a judge whose complete evidence would not. With nothing
  missing both bounds are the observed interval's. (The separate
  `max_stability_missing_rate` check can still `FAIL` on missingness
  itself.) Set `NULL` explicitly when stability is outside this policy.

- min_stability_n:

  Minimum independent items with at least two stability judgments
  (`n_items_scored`; items counted only by the worst or best case do not
  count). Required when `max_pairwise_disagreement` is not `NULL` and
  rejected otherwise.

- min_stability_runs:

  Minimum repeated judge runs. Required when `max_pairwise_disagreement`
  is not `NULL` and rejected otherwise.

- max_stability_missing_rate:

  Maximum fraction of individual repeated judgments that may be missing.
  Below this rate missing judgments still count against the disagreement
  check: its upper bound treats each one as disagreeing with its item's
  other runs (see `max_pairwise_disagreement`), so a judge cannot pass
  by abstaining on the items where it is unstable, and its lower bound
  gives each the label that least disagrees with its item's other runs,
  so abstaining cannot fail a judge either. Only cells of the run matrix
  are counted: an item or run absent from the evidence entirely is not
  missing. Required when stability is in the policy and rejected
  otherwise.

- required_probes:

  Bias probes that must be present and free of detected signal.

- min_probe_n:

  Minimum scored items required for every required bias probe: the
  probe's `n_scored`, the items with a decided judgment in both
  conditions (see
  [`probe_bias()`](https://jsizzler.github.io/yourhonor/reference/probe_bias.md)).
  It counts items, not judgments (`n_judgments`) or condition pairs
  (`n_comparisons`): a position probe over 30 pairs at `repeats = 3` has
  `n_comparisons = 90` but at most 30 scored items. Required when
  `required_probes` is non-empty and rejected otherwise.

- max_probe_flip_rate:

  Maximum upper confidence bound for a required probe's flip rate. The
  upper bound assessed counts every unscored item (one without a usable
  judgment in both conditions; for the position probe this includes
  items lost to a within-condition tie) as a flip over all `n_input`
  items, so it can exceed the probe's own `conf_high`. The lower bound
  leaves out the `n_unscored_tied` items lost to a within-condition tie
  no answer could break, which are decided evidence rather than missing
  and stay out of it as they do of the scored items' `conf_low`, and
  counts every other unscored item (each has a missing judgment) as
  unflipped, over the remaining items (it is never above that
  `conf_low`). Missing judgments can therefore turn this check's `PASS`
  or `FAIL` into `INDETERMINATE`: a `FAIL` holds however the unscored
  items they leave would have been resolved. For the position probe this
  covers unscored items only: with `repeats >= 2` a scored item can also
  have missing repeats, and both bounds take its modal choice as
  observed, so missing repeats there can still cause a `FAIL` or, within
  the missing-rate tolerance, turn a deserved `FAIL` into a `PASS`. (The
  separate `max_probe_missing_rate` check can still `FAIL` on
  missingness itself.) Required when `required_probes` is non-empty and
  rejected otherwise.

- max_probe_directional_rate:

  Maximum practically tolerable net directional effect, assessed against
  a conservative confidence interval whose upper bound counts every
  unscored item towards the worse direction, and whose lower bound
  resolves the unscored items that `max_probe_flip_rate`'s lower bound
  completes (every one not in `n_unscored_tied`) in the way that most
  cancels the observed direction (as flips the other way, or unflipped),
  over the same `n_input - n_unscored_tied` items. Required when
  `required_probes` is non-empty and rejected otherwise.

- max_probe_missing_rate:

  Maximum fraction of individual probe judgments that may be missing.
  Required when `required_probes` is non-empty so selective abstention
  cannot make a probe look clean; rejected otherwise.

- min_precision:

  Optional minimum lower confidence bound for per-class precision,
  scalar or named.

- min_human_reliability_lower:

  Optional minimum lower confidence bound for human-rater Gwet AC1
  (nominal) or AC2 (weighted). When supplied, a single-rater gold set is
  `NOT_ASSESSED`, never accepted silently.

- conf_level:

  Confidence level required across calibration, bias, and stability
  evidence.

- coefficient:

  Agreement coefficient used by the policy. `NULL` (the default) selects
  the calibration's Gwet coefficient: `"gwet_ac1"` for an unweighted
  (nominal) calibration and `"gwet_ac2"` for a weighted (ordinal) one.
  When the calibration did not compute that default coefficient, the
  agreement check is `NOT_ASSESSED` with the reason, and the other
  checks are still assessed. A coefficient named here that the
  calibration did not compute is an error when the policy is assessed,
  never a silent `NOT_ASSESSED`.

- max_abstention_rate:

  Optional maximum upper confidence bound for the judge's abstention
  rate: the share of items with a human reference that the judge left
  `NA`. See the "Missing labels and abstentions" section of
  [`calibrate_judge()`](https://jsizzler.github.io/yourhonor/reference/calibrate_judge.md).

## Value

A `yourhonor_policy` object.

## Details

Stability and bias thresholds are grouped behind a parent gate:
`max_pairwise_disagreement` switches the stability checks on and
`required_probes` switches the bias checks on. The dependent thresholds
must be supplied together with their gate and are rejected without it,
so a policy can never carry a threshold that the assessment does not
read.

A count or rate a check needs that is not a finite number (an `NA`
missing rate, say) makes that check `INDETERMINATE`: unavailable
evidence neither passes nor fails.

## Examples

``` r
# A calibration-only policy: stability and bias are outside it.
policy <- acceptance_policy(
  min_agreement_lower = 0.6, min_n = 5, min_class_n = 2,
  min_recall = 0.6, max_missing_rate = 0.05,
  max_pairwise_disagreement = NULL
)
policy
#> 
#> ── yourhonor acceptance policy ─────────────────────────────────────────────────
#> Agreement coefficient: the calibration's Gwet coefficient (gwet_ac1 unweighted,
#> gwet_ac2 weighted).
#> Confidence level: 0.95.
#> Calibration:
#> • agreement lower >= 0.6
#> • usable items >= 5
#> • class support >= 2
#> • recall lower >= 0.6
#> • missing rate <= 0.05
#> Stability: outside the policy.
#> Bias: outside the policy.
#> Assess it with `report(calibration, policy = <this policy>)`.

# Eight items meet the count floors but are too few for the interval
# checks to pass.
cal <- calibrate_judge(support_gold, label_levels = c("I", "P", "C"),
                       n_boot = 200, seed = 1)
report(cal, policy = policy)$checks
#> # A tibble: 8 × 6
#>   check                        status         value op       threshold reason   
#>   <chr>                        <chr>          <dbl> <chr>        <dbl> <chr>    
#> 1 usable_items                 PASS          8      >=            5    Sample s…
#> 2 calibration_confidence_level PASS          0.95   =             0.95 Confiden…
#> 3 calibration_missing_rate     PASS          0      <=            0.05 Criterio…
#> 4 agreement_gwet_ac1           INDETERMINATE 0.0795 lower >=      0.6  Interval…
#> 5 class_support                PASS          2      >=            2    All clas…
#> 6 recall_I                     INDETERMINATE 0.0126 lower >=      0.6  Interval…
#> 7 recall_P                     INDETERMINATE 0.0126 lower >=      0.6  Interval…
#> 8 recall_C                     INDETERMINATE 0.398  lower >=      0.6  Interval…

# Switching a gate on requires its dependent thresholds; class-specific
# recall floors are a named vector.
acceptance_policy(
  min_agreement_lower = 0.6, min_n = 50, min_class_n = 10,
  min_recall = c(I = 0.5, C = 0.7), max_missing_rate = 0.05,
  max_pairwise_disagreement = 0.1, min_stability_n = 50,
  min_stability_runs = 3, max_stability_missing_rate = 0.01,
  required_probes = "verbosity", min_probe_n = 50,
  max_probe_flip_rate = 0.1, max_probe_directional_rate = 0.05,
  max_probe_missing_rate = 0.01
)
#> 
#> ── yourhonor acceptance policy ─────────────────────────────────────────────────
#> Agreement coefficient: the calibration's Gwet coefficient (gwet_ac1 unweighted,
#> gwet_ac2 weighted).
#> Confidence level: 0.95.
#> Calibration:
#> • agreement lower >= 0.6
#> • usable items >= 50
#> • class support >= 10
#> • recall (I) lower >= 0.5
#> • recall (C) lower >= 0.7
#> • missing rate <= 0.05
#> Stability:
#> • pairwise disagreement upper <= 0.1
#> • items >= 50
#> • runs >= 3
#> • missing rate <= 0.01
#> Bias (required probes: "verbosity"):
#> • scored items >= 50
#> • flip rate upper <= 0.1
#> • directional rate upper <= 0.05
#> • missing rate <= 0.01
#> Assess it with `report(calibration, policy = <this policy>)`.
```
