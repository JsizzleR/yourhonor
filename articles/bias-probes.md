# Bias probes

``` r

library(yourhonor)
```

Calibration compares a judge with humans on the items you have. A bias
probe asks a different question: does the judge’s label move when the
input changes in a way that should not matter? This article probes the
synthetic judge from
[`vignette("yourhonor")`](https://jsizzler.github.io/yourhonor/articles/yourhonor.md),
whose planted flaw is that any input longer than 240 characters is
graded one level more generously. No input in `synthetic_gold` is that
long, so calibration could not see the flaw.

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

## Probe pointwise bias

Pointwise probes score control and treatment rows in one randomized
batch. The verbosity probe pads each answer with filler; the
self-preference and bandwagon probes prepend a short framing sentence.
`no_signal` means no directional effect was detected by that probe; it
is not a claim that bias is absent.

``` r

bias <- probe_bias(
  synthetic_gold,
  keyword_judge,
  input_cols = c("input", "target"),
  probes = c("verbosity", "self_preference", "bandwagon"),
  seed = 1,
  audit_id = "synthetic-judge-v1"
)
bias[, c("probe", "directional_rate", "direction", "p_value_adj", "verdict")]
#> 
#> ── yourhonor bias probes ──
#> 
#> # A tibble: 3 × 5
#>   probe           directional_rate direction        p_value_adj verdict  
#>   <chr>                      <dbl> <chr>                  <dbl> <chr>    
#> 1 verbosity                  0.365 favors_perturbed    2.48e-16 BIAS     
#> 2 self_preference            0.035 favors_perturbed    2.87e- 1 no_signal
#> 3 bandwagon                  0.025 favors_perturbed    2.87e- 1 no_signal
```

The padded answers cross the judge’s 240-character threshold, so the
verbosity probe’s verdict is `BIAS`: labels move towards the perturbed
(longer) version. The short framing prefixes do not cross it, so the
self-preference and bandwagon probes report both `no_signal`. A `BIAS`
verdict needs both a small adjusted p-value and a `directional_rate`
above `flip_threshold`. `flip_rate` counts a label change in either
direction and `directional_rate` only the net movement one way, so a
large but balanced `flip_rate` is reported but never flagged:

``` r

bias[, c("probe", "flip_rate", "directional_rate", "verdict")]
#> ── yourhonor bias probes ──
#> 
#> # A tibble: 3 × 4
#>   probe           flip_rate directional_rate verdict  
#>   <chr>               <dbl>            <dbl> <chr>    
#> 1 verbosity           0.435            0.365 BIAS     
#> 2 self_preference     0.085            0.035 no_signal
#> 3 bandwagon           0.055            0.025 no_signal
```

## Missing and unscored items

Missingness is reported on two denominators. `missing_rate` is the share
of individual judgments that came back `NA` and is gated directly;
`unscored_rate` is the share of items that could not be scored, which
for the position probe also includes within-condition ties. A policy’s
upper bounds count every unscored item against the judge, over all
supplied items: as a flip in the flip-rate bound, and as a flip in
whichever single direction gives the larger bound in the directional
one.

``` r

bias[, c("probe", "n_scored", "n_unscored", "unscored_rate", "missing_rate")]
#> ── yourhonor bias probes ──
#> 
#> # A tibble: 3 × 5
#>   probe           n_scored n_unscored unscored_rate missing_rate
#>   <chr>              <int>      <int>         <dbl>        <dbl>
#> 1 verbosity            200          0             0            0
#> 2 self_preference      200          0             0            0
#> 3 bandwagon            200          0             0            0
```

[`?probe_bias`](https://jsizzler.github.io/yourhonor/reference/probe_bias.md)
documents every column and its denominator.

## Probe position bias

Position bias uses a separate structured API for a *pairwise* judge.
Source columns are swapped and shown to the judge only as neutral
`.candidate_1` and `.candidate_2` slots, then display choices are mapped
back to stable candidate identity. A pairwise judge is a different
instrument from the pointwise judge audited above, so it gets its own
`audit_id`.

``` r

pairs <- tibble::tibble(
  question = c("2 + 2?", "Capital of France?"),
  candidate_a = c("4", "Paris"),
  candidate_b = c("5", "Berlin")
)
always_first <- function(data) rep("A", nrow(data))
position <- probe_position(
  pairs,
  always_first,
  candidate_a_col = "candidate_a",
  candidate_b_col = "candidate_b",
  context_cols = "question",
  repeats = 2,
  seed = 1,
  audit_id = "pairwise-judge-v1"
)
position[, c("probe", "flip_rate", "position_preference", "direction", "verdict")]
#> ── yourhonor bias probes ──
#> 
#> # A tibble: 1 × 5
#>   probe    flip_rate position_preference direction    verdict  
#>   <chr>        <dbl>               <dbl> <chr>        <chr>    
#> 1 position         1                   1 favors_first no_signal
```

Every item flipped with the display order and the preference is entirely
for the first slot, yet the verdict is `no_signal`: 2 items cannot reach
significance in a sign test. A real position audit needs a proper item
count, which is what `min_probe_n` in a policy enforces.

## Evidence for one judge only

Because its `audit_id` differs,
[`report()`](https://jsizzler.github.io/yourhonor/reference/report.md)
refuses to bind the position evidence to a calibration of the pointwise
judge rather than silently assessing two judges as one:

``` r

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
  audit_id = "synthetic-judge-v1"
)
report(cal, bias = position)
#> Error in `report()`:
#> ! Calibration, bias, and stability `audit_id` values do not match.
```

The pointwise probes share the calibration’s `audit_id`, so
`report(cal, bias = bias)` binds them;
[`vignette("policies-audit")`](https://jsizzler.github.io/yourhonor/articles/policies-audit.md)
gates them with a policy.
