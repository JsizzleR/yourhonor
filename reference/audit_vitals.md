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
