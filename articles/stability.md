# Stability and vitals

``` r

library(yourhonor)
```

A judge that gives the same item a different label on a re-run is a
noisy instrument, however well it agrees with humans on average. This
article measures the repeatability of the synthetic judge from
[`vignette("yourhonor")`](https://jsizzler.github.io/yourhonor/articles/yourhonor.md),
then shows how to reuse grades a vitals task already holds instead of
calling a model again.

``` r

keyword_judge <- function(data) {
  hedged <- grepl("probably|somewhere|should be|I think|I believe|might|sometimes|could", data$input)
  concrete <- grepl("Answer: (Open|Use|Yes)|click", data$input)
  label <- ifelse(hedged, "P", ifelse(concrete, "C", "I"))
  # The flaw under audit: a long input reads as one level more complete.
  long <- nchar(data$input) > 240
  label[long & label == "P"] <- "C"
  label[long & label == "I"] <- "P"
  # A little run-to-run error, so repeated runs are not identical.
  noise <- runif(nrow(data)) < 0.05
  label[noise] <- sample(c("I", "P", "C"), sum(noise), replace = TRUE)
  tibble::tibble(.item_id = data$.item_id, label = label)
}
```

## Measure repeatability

[`test_retest()`](https://jsizzler.github.io/yourhonor/reference/test_retest.md)
grades the same items `n_runs` times, each run under a fresh shuffle and
key. The primary stability quantity is mean pairwise disagreement across
runs. Its interval is a hybrid: the percentile item bootstrap widened to
include a Wilson score interval for the mean per-item disagreement, so a
sample with no observed disagreement never yields a zero-width interval.
The Wilson bound usually sets both ends once there are more than two
runs; the interval can only widen, never narrow. A missing judgment is
left out of that interval, so a policy does not gate it alone: it
assesses a worst-case upper bound in which every missing judgment
disagrees with its item’s other runs, so a judge cannot pass by
abstaining where it is unstable, and a best-case lower bound in which
each missing judgment takes the label that least disagrees with its
item’s other runs, so abstaining on runs that agree cannot fail a judge
either. With nothing missing both are the interval’s own bounds. The
any-flip rate is also shown but is explicitly tied to the chosen number
of runs.

The labels are ordered, so the chance-corrected stability coefficient is
weighted like the calibration: with `weights = "ordinal"` it is Gwet’s
AC2 under ordinal weights, and a re-run that moves an item from `I` to
`C` costs it more than one that moves it from `I` to `P`. Weighted
stability needs `label_levels` to know the order. Pairwise disagreement,
the quantity a policy gates, counts any change of label and is the same
under every weighting.

``` r

stability <- test_retest(
  synthetic_gold,
  keyword_judge,
  input_cols = c("input", "target"),
  n_runs = 5,
  label_levels = c("I", "P", "C"),
  weights = "ordinal",
  n_boot = 500,
  seed = 1,
  audit_id = "synthetic-judge-v1"
)
stability
#> 
#> ── yourhonor test-retest stability ──
#> 
#> 5 runs - pairwise disagreement 0.07 [0.05, 0.12].
#> gwet_ac2: 0.90 [0.87, 0.94].
#> Any-flip rate at exactly 5 runs: 0.18; missing judgments: 0.00.
```

`seed` fixes R’s random state for the shuffles (and for this synthetic
judge’s noise). It does not make a remote model deterministic:
run-to-run variation in a real judge is exactly what this measures. A
policy gates it with `max_pairwise_disagreement`, `min_stability_n`,
`min_stability_runs` and `max_stability_missing_rate`;
[`vignette("policies-audit")`](https://jsizzler.github.io/yourhonor/articles/policies-audit.md)
applies them. The policy does not take the stored bounds on trust: it
replays the bootstrap from the object’s `runs` and the random-number
state the bootstrap started from (`bootstrap_seed`), and evidence whose
stored bounds or counts do not match that rebuild, or that cannot be
rebuilt, reads `INDETERMINATE` on every stability check.

## Reuse vitals scores

[`audit_vitals()`](https://jsizzler.github.io/yourhonor/reference/audit_vitals.md)
reads `task$get_samples()$score` and joins human labels by ID; it does
not invoke the scorer again. A multi-epoch vitals task must be filtered
to one epoch for calibration because repeated grades are not independent
gold items.
[`stability_vitals()`](https://jsizzler.github.io/yourhonor/reference/stability_vitals.md)
can reuse epochs only after verifying that every judge-visible content
column stayed invariant. The chunk below is not run; it needs a graded
`vitals::Task` and a human review table.

``` r

human_review <- tibble::tibble(id = review$id, human_label = review$label)
cal <- audit_vitals(
  task,
  human_labels = human_review,
  label_levels = c("I", "P", "C"),
  scale = "ordinal",
  provenance = list(judge_prompt = "grader-v3"),
  audit_id = "support-judge-v3-gold-2026-08",
  seed = 1
)
stability <- stability_vitals(
  task,
  content_cols = c("input", "target", "result"),
  label_levels = c("I", "P", "C"),
  weights = "ordinal",
  audit_id = "support-judge-v3-gold-2026-08",
  seed = 1
)
report(cal, stability = stability)
```

Both pieces of evidence carry the same `audit_id`, so
[`report()`](https://jsizzler.github.io/yourhonor/reference/report.md)
binds them; passing a `policy` turns the report into a decision.
