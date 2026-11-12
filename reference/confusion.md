# Judge-versus-human confusion matrix

Shows where a judge and humans disagree. Label order is preserved from
`label_levels` rather than sorted alphabetically.

## Usage

``` r
confusion(
  x,
  normalize = c("none", "row", "col"),
  label_levels = NULL,
  conf_level = 0.95,
  human_col = NULL,
  judge_col = NULL
)
```

## Arguments

- x:

  A `yourhonor_calibration` object or a ratings data frame holding one
  human column and one judge column (see `human_col` and `judge_col`).

- normalize:

  One of `"none"`, `"row"`, or `"col"`.

- label_levels:

  Complete label universe in display order. A calibration object's
  stored levels are used by default.

- conf_level:

  Confidence level for exact per-class binomial intervals.

- human_col, judge_col:

  Names of the human and judge columns of a ratings data frame. Both
  must be given together. When both are `NULL`, columns named `human`
  and `judge` are used if present; otherwise a data frame with exactly
  two columns is read as human then judge. A message (class
  `yourhonor_inferred_columns`) reports the choice whenever inference
  ignored columns or read them positionally. Cannot be combined with a
  calibration object, which carries its own pairing.

## Value

A tibble with `human_label`, `judge_label`, `n`, and `prop`, in
`label_levels` order. `prop` is `NA` where its denominator is zero (the
row of a human label never observed under `normalize = "row"`, or the
column of a judge label never used under `normalize = "col"`). The
resolved level order is attached as the `"label_levels"` attribute.
Per-class metrics remain attached for compatibility and are also
available through
[`confusion_metrics()`](https://jsizzler.github.io/yourhonor/reference/confusion_metrics.md).

## Examples

``` r
ratings <- data.frame(
  human = c("C", "C", "P", "I"),
  judge = c("C", "P", "P", "I")
)
confusion(ratings, normalize = "row", label_levels = c("I", "P", "C"),
          human_col = "human", judge_col = "judge")
#> # A tibble: 9 × 4
#>   human_label judge_label     n  prop
#>   <chr>       <chr>       <int> <dbl>
#> 1 I           I               1   1  
#> 2 P           I               0   0  
#> 3 C           I               0   0  
#> 4 I           P               0   0  
#> 5 P           P               1   1  
#> 6 C           P               1   0.5
#> 7 I           C               0   0  
#> 8 P           C               0   0  
#> 9 C           C               1   0.5
```
