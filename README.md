# yourhonor

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

**Your honor depends on the judge being accurate.** `yourhonor` audits an
LLM-as-judge as a measurement instrument before its grades are used to make a
decision.

The package is deliberately R-native. This is not a claim of category novelty:
Python users should evaluate [judgecal](https://pypi.org/project/judgecal/),
which already provides clustered inference, paired testing, power analysis,
bias validation, and batch/Inspect workflows. The narrower
[judgy](https://pypi.org/project/judgy/) estimates corrected success rates from
human and test labels. A straight Python port of `yourhonor` would add little.

`yourhonor` instead fills a narrower gap around the
[ellmer](https://ellmer.tidyverse.org) and
[vitals](https://vitals.tidyverse.org) workflow: blind calibration, uncertainty,
structured bias probes, explicit acceptance policy, and a language-neutral JSON
audit summary. It does not replace an evaluation runner and it does not call a
model unless you give it a judge function.

## What it adds

- `calibrate_judge()` compares judge grades with one or more human raters. A
  live judge sees only the columns explicitly listed in `input_cols`, never the
  human labels. Rows are shuffled before opaque keys are assigned; keyed output
  is validated and restored, and the result keeps the judge-side key so logs
  can be joined back without un-blinding the run.
- `agreement()` reports Cohen's kappa, Krippendorff's alpha, and Gwet's AC1
  (nominal) or AC2 (weighted) with item-bootstrap intervals, fixed category
  levels, and explicit missingness. Weighted analyses require a declared order.
- `confusion_metrics()` reports class support, precision, and recall with exact
  intervals. This prevents a high aggregate agreement score from hiding failure
  on a rare but important class.
- `probe_bias()` tests pointwise verbosity, self-preference, and bandwagon
  effects. `probe_position()` swaps separate candidate columns and maps display
  choices from neutral display slots back to stable candidate identity. Both
  return the same schema, record raw missingness, and report conservative
  directional-effect intervals.
- `test_retest()` reports pairwise run disagreement with a conservative
  interval, rather than relying on an any-flip rate that rises mechanically
  with the number of runs.
- `acceptance_policy()` makes thresholds explicit. `report()` returns a
  `yourhonor_report` whose `$decision` is `PASS`, `FAIL`, `NOT_ASSESSED`, or
  `INDETERMINATE`; without a policy, it is descriptive and never implies that
  missing evidence passed.
- `audit_vitals()` consumes grades already present in a vitals task without
  paying for another model call. `stability_vitals()` handles invariant content
  across repeated epochs. `write_audit()` writes Markdown or a versioned
  aggregate JSON summary without automatically serializing item rows, and
  never overwrites an existing file unless told to.

## Installation

```r
# install.packages("pak")
pak::pak("JsizzleR/yourhonor")
```

## Calibrate precomputed grades

```r
library(yourhonor)

gold <- tibble::tibble(
  human_label = c("C", "C", "P", "I", "C", "P", "I", "C"),
  judge_label = c("C", "C", "C", "I", "C", "P", "P", "C")
)

cal <- calibrate_judge(
  gold,
  label_levels = c("I", "P", "C"),
  scale = "ordinal",
  n_boot = 500,
  seed = 1
)
cal
#> ── yourhonor judge calibration ─────────────────────────────────────────────────
#> 8 usable items of 8 - descriptive judge-vs-human agreement:
#> • cohen_kappa: 0.76 [0.25, 1.00] (substantial)
#> • krippendorff_alpha: 0.77 [0.18, 1.00] (substantial)
#> • gwet_ac2: 0.81 [0.56, 1.00] (almost perfect)
#> 2 disagreements; inspect `confusion_metrics()` before use.
#> ℹ No deployment decision is implied; call report() with a policy to assess one.

agreement(cal)
confusion_metrics(cal)
#> # A tibble: 3 × 9
#>   label support predicted precision precision_conf_low precision_conf_high
#>   <chr>   <int>     <int>     <dbl>              <dbl>               <dbl>
#> 1 I           2         1       1               0.0250               1    
#> 2 P           2         2       0.5             0.0126               0.987
#> 3 C           4         5       0.8             0.284                0.995
#> # ℹ 3 more variables: recall <dbl>, recall_conf_low <dbl>,
#> #   recall_conf_high <dbl>

# Evidence alone is not a deployment decision.
rep <- report(cal, output = "md")
rep$decision
#> [1] "NOT_ASSESSED"
```

`report()` returns the same `yourhonor_report` object whichever `output` you
choose; `output` only sets how it prints. `rep$checks` is a tibble with one row
per policy check, and `format(rep, output = "md")` gives the Markdown lines.

Declare thresholds from the task's error costs, not from a package-wide notion
of a "good" score:

```r
policy <- acceptance_policy(
  min_agreement_lower = 0.75,
  min_n = 100,
  min_class_n = 20,
  min_recall = c(I = 0.90, P = 0.75, C = 0.90),
  min_precision = c(C = 0.90),
  max_missing_rate = 0.01,
  max_pairwise_disagreement = 0.05,
  min_stability_n = 100,
  min_stability_runs = 3,
  max_stability_missing_rate = 0.01,
  required_probes = c("verbosity", "position"),
  min_probe_n = 50,
  max_probe_flip_rate = 0.05,
  max_probe_directional_rate = 0.05,
  max_probe_missing_rate = 0.01,
  conf_level = 0.95,
  coefficient = "gwet_ac2"
)

# Missing required probes or stability are shown as NOT_ASSESSED.
report(cal, policy = policy, output = "console")
#> ── yourhonor assessment: NOT_ASSESSED ──────────────────────────────────────────
#> Judge evidence: n = 8 of 8.
#> Agreement evidence: gwet_ac2 = 0.81 [0.56, 1.00] (almost perfect).
#> Per-class evidence:
#> • "I": support 2, precision 1.00, recall 0.50
#> • "P": support 2, precision 0.50, recall 0.50
#> • "C": support 4, precision 0.80, recall 1.00
#> Bias probes: NOT ASSESSED.
#> Test-retest stability: NOT ASSESSED.
#> 
#> ── policy checks ──
#> 
#> INDETERMINATE: usable_items = 8 (>= 100) - Sample size is below the declared
#> minimum.
#> PASS: calibration_confidence_level = 0.95 (= 0.95) - Confidence level matches
#> policy.
#> PASS: calibration_missing_rate = 0 (<= 0.01) - Criterion met.
#> INDETERMINATE: agreement_gwet_ac2 = 0.557 (lower >= 0.75) - Interval overlaps
#> threshold.
#> INDETERMINATE: class_support = 2 (>= 20) - Insufficient reference support for:
#> I, P, C
#> INDETERMINATE: recall_I = 0.0126 (lower >= 0.9) - Interval overlaps threshold.
#> INDETERMINATE: recall_P = 0.0126 (lower >= 0.75) - Interval overlaps threshold.
#> INDETERMINATE: recall_C = 0.398 (lower >= 0.9) - Interval overlaps threshold.
#> INDETERMINATE: precision_C = 0.284 (lower >= 0.9) - Interval overlaps
#> threshold.
#> NOT_ASSESSED: bias_verbosity = NA (upper <= 0.05) - Required bias probe was not
#> supplied.
#> NOT_ASSESSED: bias_position = NA (upper <= 0.05) - Required bias probe was not
#> supplied.
#> NOT_ASSESSED: stability_pairwise_disagreement = NA (upper <= 0.05) - Required
#> stability evidence was not supplied.
```

Stability and probe thresholds live behind their gates
(`max_pairwise_disagreement` and `required_probes`). Supplying one without its
gate is an error, so a policy never carries a threshold that is not assessed.

## Calibrate a live judge without label leakage

A judge function receives a shuffled, redacted data frame with a fresh opaque
`.item_id`. Returning that key with the label lets `yourhonor` verify and restore
row order. A caller ID is never exposed unless it is explicitly listed in
`input_cols`.

```r
model_judge <- function(data) {
  # Replace this body with ellmer calls using data$input and data$target.
  tibble::tibble(
    .item_id = data$.item_id,
    label = rep("C", nrow(data))
  )
}

cal <- calibrate_judge(
  support_gold,
  judge = model_judge,
  input_cols = c("input", "target"),
  id_col = "id",
  label_levels = c("I", "P", "C"),
  scale = "ordinal",
  provenance = list(model = "provider/model", prompt_version = "judge-v3"),
  audit_id = "support-judge-v3-gold-2026-08",
  seed = 1
)
```

`input_cols` is required for a live judge. Human-rating columns and an existing
judge-label column are rejected if included, making leakage an API error rather
than a convention. The `.item_id` the judge sees is assigned after shuffling;
`cal$items$.judge_item_id` records it so judge-side logs can be joined back.

## Position bias needs structured candidates

Position is not a text-regex perturbation. Keep candidates separate so their
identity survives the swap:

```r
pairs <- tibble::tibble(
  question = c("2 + 2?", "Capital of France?"),
  candidate_a = c("4", "Paris"),
  candidate_b = c("5", "Berlin")
)

pairwise_judge <- function(data) {
  # Candidate text is in neutral .candidate_1/.candidate_2 display slots.
  # Return A/B in display space. This example always picks the first candidate.
  rep("A", nrow(data))
}

probe_position(
  pairs,
  pairwise_judge,
  candidate_a_col = "candidate_a",
  candidate_b_col = "candidate_b",
  context_cols = "question",
  repeats = 2,
  seed = 1,
  audit_id = "pairwise-judge-v1"
)
#> ── yourhonor bias probes ──
#> 
#> # A tibble: 1 × 32
#>   probe    flip_rate conf_low conf_high direction    directional_rate
#>   <chr>        <dbl>    <dbl>     <dbl> <chr>                   <dbl>
#> 1 position         1    0.158         1 favors_first                1
#> # ℹ 26 more variables: directional_conf_low <dbl>, directional_conf_high <dbl>,
#> #   position_preference <dbl>, display_first_rate <dbl>,
#> #   position_conf_low <dbl>, position_conf_high <dbl>, statistic <int>,
#> #   n_discordant <int>, n_scored <int>, n_directional_items <int>,
#> #   n_input <int>, n_unscored <int>, unscored_rate <dbl>,
#> #   n_items_missing <int>, n_judgments <int>, n_judgments_missing <int>,
#> #   missing_rate <dbl>, n_comparisons <int>, p_value <dbl>, …
```

Pointwise and position probes share this schema; `?probe_bias` documents every
column and its denominator. Two items cannot reach significance, so the verdict
here is `no_signal` despite a total preference for the first slot: use
`min_probe_n` in a policy to require a real sample.

## Use grades already produced by vitals

```r
# task$eval() has already populated task$get_samples()$score.
human <- tibble::tibble(
  id = human_review$id,
  human_label = human_review$label
)

cal <- audit_vitals(
  task,
  human_labels = human,
  label_levels = c("I", "P", "C"),
  scale = "ordinal",
  provenance = list(judge_prompt = "grader-v3"),
  audit_id = "support-judge-v3-gold-2026-08",
  seed = 1
)

# An existing file is an error unless overwrite = TRUE.
write_audit(cal, "judge-audit.json", policy = policy, format = "json")
```

Vitals tasks with multiple epochs contain repeated IDs. Filter to one epoch for
calibration. To reuse epochs as judge-stability evidence, list every
judge-visible field and require it to remain invariant:

```r
stability <- stability_vitals(
  task,
  content_cols = c("input", "target", "result"),
  label_levels = c("I", "P", "C"),
  weights = "ordinal",
  audit_id = "support-judge-v3-gold-2026-08",
  seed = 1
)
```

If solver output changes between epochs, this adapter refuses the claim: those
rows measure end-to-end evaluation variation, not judge test-retest stability.

JSON artifacts omit `cal$items`, but caller-supplied provenance is written
verbatim. Do not place secrets or raw item content in provenance fields. The
`yourhonor.audit.v1` schema is documented in `?write_audit`: numbers are
written at full precision (17 significant digits, so every double round-trips
exactly), absent values are `null`, and vector fields are always arrays.

## Working with raw rating frames

`agreement()`, `confusion()`, and `plot_confusion()` also accept a plain data
frame. Name the columns explicitly (`rater_cols` for `agreement()`,
`human_col`/`judge_col` for `confusion()`); when you do not, columns named
`human` and `judge` are used if both exist, and any other inference is
announced with a message so a wide frame is never silently misread:

```r
ratings <- data.frame(
  id = 1:4,
  human = c("C", "C", "P", "I"),
  judge = c("C", "P", "P", "I")
)
confusion(ratings, label_levels = c("I", "P", "C"))
#> ℹ Using human and judge as the rating columns.
#> ℹ Ignoring column: id.
#>   Set `human_col` and `judge_col` to choose explicitly.
#> # A tibble: 9 × 4
#>   human_label judge_label     n  prop
#>   <chr>       <chr>       <int> <dbl>
#> 1 I           I               1  0.25
#> 2 P           I               0  0   
#> 3 C           I               0  0   
#> 4 I           P               0  0   
#> 5 P           P               1  0.25
#> 6 C           P               1  0.25
#> 7 I           C               0  0   
#> 8 P           C               0  0   
#> 9 C           C               1  0.25
```

## Interpreting the evidence

- Human-human agreement is a reliability benchmark, not a ceiling. A judge can
  agree with a consensus more strongly than individual humans agree with each
  other.
- With several human raters, each item's reference label is the modal (most
  frequent) human label. Items whose top labels tie have no consensus and are
  excluded from the judge-versus-consensus evidence (`n_consensus_ties`). With
  two raters every disagreement is a tie, so the judge is scored only on the
  items humans agreed about; that direction is optimistic. `per_rater_agreement`
  compares the judge against each rater on every item.
- The test-retest pairwise-disagreement interval is a hybrid: the percentile
  item bootstrap widened to include a Wilson score interval for the mean
  per-item disagreement. It keeps a zero-disagreement sample from producing a
  zero-width interval, usually sets both bounds once there are more than two
  runs, and can only widen the bootstrap, never narrow it. The chance-corrected
  stability interval is the plain item bootstrap.
- `no_signal` from a bias probe means the probe did not detect a directional
  effect. It is not proof that the judge is unbiased. A `BIAS` verdict needs
  both a small adjusted p-value and a net `directional_rate` above
  `flip_threshold`; a large but balanced flip rate is reported, not flagged.
- A single observed class cannot identify chance-corrected agreement. Such
  results are `INDETERMINATE`, not perfect.
- A finite sample with empirically perfect varied agreement produces a
  zero-width item bootstrap. `yourhonor` flags that interval as unreliable
  rather than treating `[1, 1]` as population certainty.
- Aggregate agreement never substitutes for class-specific error requirements.

## Statistical scope

- Calibration and stability intervals treat rows/items as independent sampling
  units. Repeated templates, conversations, users, or other clusters must be
  reduced to independent units or analyzed with a cluster-aware method outside
  this package. Vitals epochs are repeated measurements, not extra items.
- Freeze a representative, held-out gold set before tuning the judge or prompt.
  Record dataset split/version/hash and model/prompt versions in `provenance`.
  Reusing a tuning set is not an independent audit.
- Reported confidence levels apply to each interval, not to a simultaneous band
  covering every displayed estimate. Policy acceptance is an all-criteria gate.
- `support_gold` is an eight-item synthetic API demonstration, not a benchmark
  or a basis for a deployment decision.

See `vignette("yourhonor")` for an end-to-end walkthrough.

## Status

Experimental and pre-release. The core audit surface is implemented and covered
by regression tests. The package delegates agreement estimators to
[irrCAC](https://cran.r-project.org/package=irrCAC); its value is the judge-safe
workflow, conservative evidence model, and R/vitals integration around those
estimators. JSON files are portable aggregate audit summaries; independent
reproduction still requires the frozen data, judge configuration, and code.

## License

MIT © yourhonor authors
