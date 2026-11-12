# Audit grades from a vitals task

Extracts samples from a `vitals::Task`, joins blind human labels, and
delegates to
[`calibrate_judge()`](https://jsizzler.github.io/yourhonor/reference/calibrate_judge.md).
No model is called by this adapter.

## Usage

``` r
audit_vitals(
  task,
  human_labels,
  id_col = "id",
  human_col = "human_label",
  score_col = "score",
  provenance = list(),
  ...
)
```

## Arguments

- task:

  A `vitals::Task`-like object with a `get_samples()` method, or a
  samples data frame for testing/offline use.

- human_labels:

  A label vector aligned with samples, or a data frame keyed by `id_col`
  and containing `human_col`.

- id_col:

  Unique sample ID column.

- human_col:

  Human-label column name.

- score_col:

  Vitals score column containing judge grades.

- provenance:

  Optional audit metadata merged with adapter metadata.

- ...:

  Additional arguments passed to
  [`calibrate_judge()`](https://jsizzler.github.io/yourhonor/reference/calibrate_judge.md).

## Value

A `yourhonor_calibration` object.

## Repeated epochs

A vitals task with multiple epochs contains duplicated item IDs. This
adapter deliberately rejects those rows for calibration because treating
reruns as independent gold items would make confidence intervals too
narrow. Filter to one epoch before calling `audit_vitals()`. Use
[`stability_vitals()`](https://jsizzler.github.io/yourhonor/reference/stability_vitals.md)
for epochs only when every judge-visible content field is invariant
within item.

## Examples

``` r
# A samples data frame stands in for a vitals task here; with vitals
# installed, pass the task itself and its `get_samples()` is read.
samples <- tibble::tibble(
  id = paste0("q", 1:8),
  input = paste("question", 1:8),
  score = c("C", "C", "P", "I", "C", "P", "I", "C")
)
# Human labels keyed by sample ID, in any order.
human <- tibble::tibble(
  id = paste0("q", 8:1),
  human_label = c("C", "I", "C", "C", "I", "P", "C", "C")
)
cal <- audit_vitals(samples, human, label_levels = c("I", "P", "C"),
                    n_boot = 200, seed = 1)
cal
#> 
#> ── yourhonor judge calibration ─────────────────────────────────────────────────
#> 8 usable items of 8 - descriptive judge-vs-human agreement:
#> • cohen_kappa: 0.79 [0.00, 1.00] (substantial)
#> • krippendorff_alpha: 0.80 [-0.00, 1.00] (substantial)
#> • gwet_ac1: 0.82 [0.45, 1.00] (almost perfect)
#> 1 disagreement; inspect `confusion_metrics()` before use.
#> ℹ No deployment decision is implied; call report() with a policy to assess one.
```
