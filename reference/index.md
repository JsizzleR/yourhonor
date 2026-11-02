# Package index

## Calibrate a judge

Compare judge labels with human labels, blind to the labels.

- [`calibrate_judge()`](https://jsizzler.github.io/yourhonor/reference/calibrate_judge.md)
  : Calibrate an LLM-as-judge against human labels
- [`agreement()`](https://jsizzler.github.io/yourhonor/reference/agreement.md)
  : Chance-corrected agreement between a judge and humans
- [`confusion()`](https://jsizzler.github.io/yourhonor/reference/confusion.md)
  : Judge-versus-human confusion matrix
- [`confusion_metrics()`](https://jsizzler.github.io/yourhonor/reference/confusion_metrics.md)
  : Per-class judge metrics
- [`plot_confusion()`](https://jsizzler.github.io/yourhonor/reference/plot_confusion.md)
  : Plot a judge-versus-human confusion matrix

## Bias probes

Structured perturbations that measure directional label movement.

- [`probe_bias()`](https://jsizzler.github.io/yourhonor/reference/probe_bias.md)
  : Probe a pointwise judge for paired bias
- [`probe_position()`](https://jsizzler.github.io/yourhonor/reference/probe_position.md)
  : Probe a pairwise judge for position bias
- [`combine_bias()`](https://jsizzler.github.io/yourhonor/reference/combine_bias.md)
  : Combine bias-probe evidence

## Stability

Test-retest evidence from repeated gradings.

- [`test_retest()`](https://jsizzler.github.io/yourhonor/reference/test_retest.md)
  : Test-retest stability of a judge

## Decisions and artifacts

Explicit acceptance criteria, reports, and portable audit files.

- [`acceptance_policy()`](https://jsizzler.github.io/yourhonor/reference/acceptance_policy.md)
  : Declare a judge acceptance policy
- [`report()`](https://jsizzler.github.io/yourhonor/reference/report.md)
  : Report judge evidence or assess an explicit policy
- [`write_audit()`](https://jsizzler.github.io/yourhonor/reference/write_audit.md)
  : Write a portable judge audit artifact

## vitals integration

Reuse grades and epochs already present in a vitals task.

- [`audit_vitals()`](https://jsizzler.github.io/yourhonor/reference/audit_vitals.md)
  : Audit grades from a vitals task
- [`stability_vitals()`](https://jsizzler.github.io/yourhonor/reference/stability_vitals.md)
  : Measure stability from precomputed vitals epochs

## Data

- [`support_gold`](https://jsizzler.github.io/yourhonor/reference/support_gold.md)
  : Example gold-labeled support answers
- [`synthetic_gold`](https://jsizzler.github.io/yourhonor/reference/synthetic_gold.md)
  : Synthetic calibration set with three human raters
