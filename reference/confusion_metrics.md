# Per-class judge metrics

Returns class support, predicted support, precision, and recall with
humans treated as the reference. Unlike a hidden tibble attribute, this
accessor is stable across ordinary serialization and reporting
workflows.

## Usage

``` r
confusion_metrics(x, label_levels = NULL, human_col = NULL, judge_col = NULL)
```

## Arguments

- x:

  A `yourhonor_calibration` object, a confusion tibble returned by
  [`confusion()`](https://jsizzler.github.io/yourhonor/reference/confusion.md),
  or a ratings data frame.

- label_levels:

  Optional complete label universe in display order. For a calibration
  object or a confusion tibble it must contain every label observed
  there; the stored per-class rows are returned in that order, with
  zero-support rows for labels not seen.

- human_col, judge_col:

  Passed to
  [`confusion()`](https://jsizzler.github.io/yourhonor/reference/confusion.md)
  for a ratings data frame. Cannot be combined with a calibration object
  or a confusion tibble.

## Value

A tibble with one row per label.
