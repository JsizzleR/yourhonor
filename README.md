# yourhonor

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

**Your honor depends on the judge being accurate.** Is your LLM-as-judge fit to
wear the robe? `yourhonor` puts it on the stand.

In R, [ellmer](https://ellmer.tidyverse.org) makes the model call and
[vitals](https://vitals.tidyverse.org) runs the eval, scoring each answer
Correct / Partial / Incorrect with a model-graded judge. But nothing tells you
whether that judge is any good. Published `vitals` walkthroughs show the
consequence: the same task scored **70% on one run and 90% on another**. If your
instrument moves 20 points run to run, you don't have a measurement — you have a
vibe.

`yourhonor` treats the judge as a measurement instrument and reports its
psychometrics. It does not run evaluations; it measures the judge that scores
them. It is the complement to `vitals`: **vitals measures the model, yourhonor
measures the ruler.**

## What it does

- **`calibrate_judge()`** — run a judge over human-labeled examples and report
  judge-vs-human agreement with the right chance-corrected coefficients (Cohen's
  kappa, Krippendorff's alpha, Gwet's AC1), each with a bootstrap confidence
  interval. (`cohen_kappa` is computed via `irrCAC::conger.kappa.raw()`, which
  equals Cohen's kappa for two raters.) Pass several `human_cols` and it also
  reports the **human-human agreement ceiling** — you can't expect a judge to
  agree with people more than they agree with each other.
- **`confusion()`** — a tidy judge-vs-human confusion matrix with per-class
  precision and recall, so you see *where* it diverges.
- **`probe_bias()`** — paired, length-matched bias probes: perturb each item in a
  way a fair judge should ignore, and measure the flip rate and direction with a
  sign test (p-values corrected across probes). `verbosity`, `self_preference` and
  `bandwagon` apply to a pointwise grader; `position` is for pairwise (A/B) judges.
- **`test_retest()`** — re-grade the same items repeatedly and quantify the
  run-to-run wobble, reported as a chance-corrected stability coefficient.
- **`report()`** — a one-paragraph "USE / USE WITH CAUTION / NOT CALIBRATED"
  verdict you can paste beside an eval.

It is built on R's mature inter-rater statistics
([irrCAC](https://cran.r-project.org/package=irrCAC)) rather than reinventing the
math — the kind of measurement rigor R does better than any other ecosystem.

## Installation

```r
# install.packages("pak")
pak::pak("JsizzleR/yourhonor")
```

## Quick start

`yourhonor` talks to a judge through one small contract: a function that takes a
data frame of items and returns one label per row. That makes it trivial to wrap
an `ellmer` chat or a `vitals` scorer — or, as here, to calibrate grades you
already have. Data leads, so a gold tibble pipes straight in.

```r
library(yourhonor)

gold <- tibble::tibble(
  human_label = c("C", "C", "P", "I", "C", "P", "I", "C"),
  judge_label = c("C", "C", "C", "I", "C", "P", "P", "C")
)

cal <- calibrate_judge(gold, seed = 1)
cal
#> ── yourhonor judge calibration ──
#> 8 items - judge-vs-human agreement (95% bootstrap CI):
#> • cohen_kappa: 0.58 [0.04, 1.00] (moderate)
#> • krippendorff_alpha: 0.60 [-0.02, 1.00] (moderate)
#> • gwet_ac1: 0.65 [0.10, 1.00] (substantial)
#> 2 disagreements; see `confusion()` for where they fall.

report(cal, output = "console")
#> ── yourhonor verdict: USE WITH CAUTION ──
#> Agreement: gwet_ac1 = 0.65 [0.10, 1.00] (substantial).
```

(The intervals are wide here because the demo has only 8 items — that honesty is
the point.) To calibrate a real model judge, pass a function instead:

```r
library(ellmer)

model_judge <- function(data) {
  chat <- chat_anthropic(system_prompt = "Grade as exactly C, P or I.")
  vapply(seq_len(nrow(data)),
         \(i) chat$clone()$chat(paste(data$target[i], data$input[i])),
         character(1))
}

support_gold |> calibrate_judge(model_judge, seed = 1)
```

See `vignette("yourhonor")` for the full walkthrough.

## How it fits the stack

```
ellmer      → makes the model call
vitals      → runs the eval, scores with a judge
yourhonor     → measures whether that judge can be trusted
```

`yourhonor` consumes the judge you already use and stays out of the eval loop. A
judge calibrated here is a judge you can defend.

## Status

Experimental and pre-release. The v1 surface above is implemented and the
package passes `R CMD check` cleanly with a full test suite. Item-response
(Rasch) judge-difficulty modeling and automatic bias *mitigation* are planned
for a later release. The agreement statistics are delegated to `irrCAC`; the
value `yourhonor` adds is the judge-native orchestration around them.

## License

MIT © yourhonor authors
