# Policies and audit artifacts

``` r

library(yourhonor)
```

Evidence is descriptive until a policy says what it must show. This
article gathers the three kinds of evidence about the synthetic judge
from
[`vignette("yourhonor")`](https://jsizzler.github.io/yourhonor/articles/yourhonor.md)
(calibration, bias probes and stability), assesses them under one
explicit policy, and writes the assessment as a portable artifact.

## Gather the evidence

The judge and the three calls are the ones
[`vignette("yourhonor")`](https://jsizzler.github.io/yourhonor/articles/yourhonor.md),
[`vignette("bias-probes")`](https://jsizzler.github.io/yourhonor/articles/bias-probes.md)
and
[`vignette("stability")`](https://jsizzler.github.io/yourhonor/articles/stability.md)
explain. Every call carries the same `audit_id`, which is what lets
[`report()`](https://jsizzler.github.io/yourhonor/reference/report.md)
bind them.

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

cal <- calibrate_judge(
  synthetic_gold,
  judge = keyword_judge,
  human_cols = c("rater_1", "rater_2", "rater_3"),
  input_cols = c("input", "target"),
  id_col = "id",
  label_levels = c("I", "P", "C"),
  scale = "ordinal",
  n_boot = 500,
  seed = 1,
  provenance = list(model = "synthetic keyword judge", prompt_version = "v1"),
  audit_id = "synthetic-judge-v1"
)
bias <- probe_bias(
  synthetic_gold,
  keyword_judge,
  input_cols = c("input", "target"),
  probes = c("verbosity", "self_preference", "bandwagon"),
  seed = 1,
  audit_id = "synthetic-judge-v1"
)
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
```

## Declare the policy

[`report()`](https://jsizzler.github.io/yourhonor/reference/report.md)
is descriptive unless it receives an acceptance policy. Policy checks
use confidence bounds and distinguish failure, missing evidence, and
statistical indeterminacy. Thresholds that only apply inside a gate
(`max_pairwise_disagreement` for stability, `required_probes` for bias)
must be supplied with that gate and are rejected without it, so a policy
never carries a threshold the assessment does not read. Declare
thresholds from the task’s error costs, not from a package-wide notion
of a “good” score.

``` r

policy <- acceptance_policy(
  min_agreement_lower = 0.75,
  min_n = 150,
  min_class_n = 20,
  min_recall = c(I = 0.80, P = 0.75, C = 0.85),
  min_precision = c(C = 0.85),
  max_missing_rate = 0.01,
  max_pairwise_disagreement = 0.10,
  min_stability_n = 150,
  min_stability_runs = 3,
  max_stability_missing_rate = 0.01,
  required_probes = c("verbosity", "self_preference", "bandwagon"),
  min_probe_n = 150,
  max_probe_flip_rate = 0.10,
  max_probe_directional_rate = 0.10,
  max_probe_missing_rate = 0.01,
  conf_level = 0.95,
  coefficient = "gwet_ac2"
)
verdict <- report(cal, bias = bias, stability = stability, policy = policy)
verdict
#> 
#> ── yourhonor assessment: FAIL ──────────────────────────────────────────────────
#> Judge evidence: n = 200 of 200.
#> Agreement with humans: gwet_ac2 = 0.97 (almost perfect); 95% bootstrap interval
#> [0.93, 1.00], exact lower bound 0.90.
#> Per-class evidence:
#> • "I": support 22, precision 0.91, recall 0.95
#> • "P": support 62, precision 0.98, recall 0.97
#> • "C": support 116, precision 0.98, recall 0.98
#> Human reliability benchmark: gwet_ac2 = 0.92.
#> Policy-failing bias checks: bias_verbosity_direction, bias_verbosity.
#> Pairwise disagreement: 0.07; 95% interval [0.05, 0.12], worst-case upper bound
#> 0.12.
#> Assessment checks:
#>   • PASS: usable_items = 200 (>= 150) - Sample size is sufficient.
#>   • PASS: calibration_confidence_level = 0.95 (= 0.95) - Confidence level
#>   matches policy.
#>   • PASS: calibration_missing_rate = 0 (<= 0.01) - Criterion met.
#>   • PASS: agreement_gwet_ac2 = 0.905 (lower >= 0.75) - Interval clears
#>   threshold. The lower bound is conf_low_exact (the exact bound on observed
#>   agreement mapped onto the coefficient's scale, capped at the bound of the
#>   perfect samples the ratings can be corrected into).
#>   • PASS: class_support = 22 (>= 20) - All classes meet support.
#>   • INDETERMINATE: recall_I = 0.772 (lower >= 0.8) - Interval overlaps
#>   threshold.
#>   • PASS: recall_P = 0.888 (lower >= 0.75) - Interval clears threshold.
#>   • PASS: recall_C = 0.939 (lower >= 0.85) - Interval clears threshold.
#>   • PASS: precision_C = 0.939 (lower >= 0.85) - Interval clears threshold.
#>   • PASS: bias_verbosity_missing_rate = 0 (<= 0.01) - Criterion met.
#>   • PASS: bias_verbosity_n = 200 (>= 150) - Sample size is sufficient.
#>   • FAIL: bias_verbosity_direction = 0.469 (upper <= 0.1) - Interval exceeds
#>   threshold.
#>   • FAIL: bias_verbosity = 0.507 (upper <= 0.1) - Interval exceeds threshold.
#>   • PASS: bias_self_preference_missing_rate = 0 (<= 0.01) - Criterion met.
#>   • PASS: bias_self_preference_n = 200 (>= 150) - Sample size is sufficient.
#>   • INDETERMINATE: bias_self_preference_direction = 0.102 (upper <= 0.1) -
#>   Interval overlaps threshold.
#>   • INDETERMINATE: bias_self_preference = 0.133 (upper <= 0.1) - Interval
#>   overlaps threshold.
#>   • PASS: bias_bandwagon_missing_rate = 0 (<= 0.01) - Criterion met.
#>   • PASS: bias_bandwagon_n = 200 (>= 150) - Sample size is sufficient.
#>   • PASS: bias_bandwagon_direction = 0.0808 (upper <= 0.1) - Interval clears
#>   threshold.
#>   • PASS: bias_bandwagon = 0.0963 (upper <= 0.1) - Interval clears threshold.
#>   • INDETERMINATE: stability_pairwise_disagreement = 0.119 (upper <= 0.1) -
#>   Interval overlaps threshold.
#>   • PASS: stability_items = 200 (>= 150) - Sample size is sufficient.
#>   • PASS: stability_runs = 5 (>= 3) - Sample size is sufficient.
#>   • PASS: stability_missing_rate = 0 (<= 0.01) - Criterion met.
```

## Read the decision

[`report()`](https://jsizzler.github.io/yourhonor/reference/report.md)
returns a `yourhonor_report` whichever `output` you choose; `output`
only sets the default rendering. `verdict$decision` is the overall
outcome, `verdict$checks` holds one row per check with the `value`
compared with the threshold (for an interval check, the bound assessed),
the comparison `op`, and the numeric `threshold`, and
`format(verdict, output = "md")` gives the Markdown lines.

``` r

verdict$decision
#> [1] "FAIL"
verdict$checks[verdict$checks$status != "PASS", c("check", "status", "value", "op", "threshold")]
#> # A tibble: 6 × 5
#>   check                           status        value op       threshold
#>   <chr>                           <chr>         <dbl> <chr>        <dbl>
#> 1 recall_I                        INDETERMINATE 0.772 lower >=       0.8
#> 2 bias_verbosity_direction        FAIL          0.469 upper <=       0.1
#> 3 bias_verbosity                  FAIL          0.507 upper <=       0.1
#> 4 bias_self_preference_direction  INDETERMINATE 0.102 upper <=       0.1
#> 5 bias_self_preference            INDETERMINATE 0.133 upper <=       0.1
#> 6 stability_pairwise_disagreement INDETERMINATE 0.119 upper <=       0.1
```

The outcome may be `PASS`, `FAIL`, `NOT_ASSESSED`, or `INDETERMINATE`.
That is intentional: absence of a probe and an underpowered probe are
different facts. Of the 25 checks here, 19 pass, 2 fail and 4 are
`INDETERMINATE`, so the decision is `FAIL`.

The failures are the planted flaw: `bias_verbosity_direction`, whose
upper bound of 0.469 must be \<= 0.1; `bias_verbosity`, whose upper
bound of 0.507 must be \<= 0.1. The verbosity probe pads answers past
the judge’s 240-character threshold, and the labels move towards the
longer version far more often than the ceiling allows.

The `INDETERMINATE` checks are `recall_I`, whose lower bound of 0.772
must be \>= 0.8; `bias_self_preference_direction`, whose upper bound of
0.102 must be \<= 0.1; `bias_self_preference`, whose upper bound of
0.133 must be \<= 0.1; `stability_pairwise_disagreement`, whose upper
bound of 0.119 must be \<= 0.1. Their intervals overlap the threshold,
so the evidence neither clears nor fails it. Recall on `I`, for
instance, rests on the 22 items whose human consensus is incorrect; a
probe that reports `no_signal` can still have an interval that does not
exclude the ceiling. None of these is a `PASS`, and none is reported as
one. Required evidence that was never supplied is `NOT_ASSESSED`, never
silently passed:

``` r

unprobed <- report(cal, stability = stability, policy = policy)
unprobed$decision
#> [1] "NOT_ASSESSED"
unprobed$checks[unprobed$checks$status == "NOT_ASSESSED", c("check", "status")]
#> # A tibble: 3 × 2
#>   check                status      
#>   <chr>                <chr>       
#> 1 bias_verbosity       NOT_ASSESSED
#> 2 bias_self_preference NOT_ASSESSED
#> 3 bias_bandwagon       NOT_ASSESSED
```

## Write an artifact

[`write_audit()`](https://jsizzler.github.io/yourhonor/reference/write_audit.md)
writes the same assessment as a Markdown report or as a versioned JSON
summary (`yourhonor.audit.v1`). An existing file is never replaced
unless `overwrite = TRUE`, so a re-run cannot clobber an earlier audit.

``` r

path <- tempfile("judge-audit-", fileext = ".json")
write_audit(cal, path, bias = bias, stability = stability, policy = policy)
```

``` r

audit <- jsonlite::read_json(path)
audit$decision
#> [1] "FAIL"
names(audit)
#>  [1] "schema_version" "created_at"     "decision"       "audit_id"      
#>  [5] "policy"         "checks"         "calibration"    "bias"          
#>  [9] "stability"      "provenance"
```

The JSON artifact contains aggregate calibration evidence, policy
checks, and provenance at full numeric precision, with `null` for every
absent value. Full precision is the stored doubles’, not the estimates’:
irrCAC rounds each agreement coefficient to five decimals, and its
bootstrap bounds inherit that. Calibration item rows are omitted, but
caller-supplied provenance is written verbatim; keep secrets and raw
item content out of provenance fields.
[`?write_audit`](https://jsizzler.github.io/yourhonor/reference/write_audit.md)
documents the schema.

## Statistical scope

The item bootstrap and exact class intervals assume independent sampling
units. Do not count repeated templates, users, conversations, or epochs
as independent items. Use a frozen, representative, held-out calibration
set that was not used to tune the judge or prompt, and record the data
split/version/hash plus the model and prompt versions in `provenance`.
Confidence levels describe each interval, not a simultaneous band over
every estimate. This synthetic example demonstrates APIs; it is not
deployment evidence.
