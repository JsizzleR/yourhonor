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
  is restored safely.
- `agreement()` reports Cohen's kappa, Krippendorff's alpha, and Gwet's AC1
  (nominal) or AC2 (weighted) with item-bootstrap intervals, fixed category
  levels, and explicit missingness. Weighted analyses require a declared order.
- `confusion_metrics()` reports class support, precision, and recall with exact
  intervals. This prevents a high aggregate agreement score from hiding failure
  on a rare but important class.
- `probe_bias()` tests pointwise verbosity, self-preference, and bandwagon
  effects. `probe_position()` swaps separate candidate columns and maps display
  choices from neutral display slots back to stable candidate identity. Both
  record raw missingness and conservative directional-effect intervals.
- `test_retest()` reports pairwise run disagreement with uncertainty, rather
  than relying on an any-flip rate that rises mechanically with the number of
  runs.
- `acceptance_policy()` makes thresholds explicit. `report()` returns `PASS`,
  `FAIL`, `NOT_ASSESSED`, or `INDETERMINATE`; without a policy, it is descriptive
  and never implies that missing evidence passed.
- `audit_vitals()` consumes grades already present in a vitals task without
  paying for another model call. `stability_vitals()` handles invariant content
  across repeated epochs. `write_audit()` writes Markdown or a versioned
  aggregate JSON summary without automatically serializing item rows.

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

agreement(cal)
confusion_metrics(cal)

# Evidence alone is not a deployment decision.
report(cal, output = "md")
attr(report(cal, output = "md"), "decision")
#> [1] "NOT_ASSESSED"
```

Declare thresholds from the task's error costs, not from a package-wide notion
of a “good” score:

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

# Missing required probes or stability will be shown as NOT_ASSESSED.
report(cal, policy = policy, output = "console")
```

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
than a convention.

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
```

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
verbatim. Do not place secrets or raw item content in provenance fields.

## Interpreting the evidence

- Human-human agreement is a reliability benchmark, not a ceiling. A judge can
  agree with a consensus more strongly than individual humans agree with each
  other.
- `no_signal` from a bias probe means the probe did not detect a directional
  effect. It is not proof that the judge is unbiased.
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
