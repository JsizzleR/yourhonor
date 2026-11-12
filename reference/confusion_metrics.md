# Per-class judge metrics

Returns class support, predicted support, precision, and recall with
humans treated as the reference. Unlike a hidden tibble attribute, this
accessor is stable across ordinary serialization and reporting
workflows.

## Usage

``` r
confusion_metrics(
  x,
  label_levels = NULL,
  human_col = NULL,
  judge_col = NULL,
  conf_level = NULL
)
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

- conf_level:

  Confidence level for the exact per-class intervals of a ratings data
  frame; `NULL` uses 0.95. A calibration object or a confusion tibble
  keeps the level its metrics were computed at, so `conf_level` cannot
  be combined with one: set it in
  [`calibrate_judge()`](https://jsizzler.github.io/yourhonor/reference/calibrate_judge.md)
  or
  [`confusion()`](https://jsizzler.github.io/yourhonor/reference/confusion.md)
  instead.

## Value

A tibble with one row per label.

## Examples

``` r
ratings <- data.frame(
  human = c("C", "C", "P", "I", "C", "P"),
  judge = c("C", "P", "P", "I", "C", "C")
)
confusion_metrics(ratings, label_levels = c("I", "P", "C"))
#> # A tibble: 3 × 9
#>   label support predicted precision precision_conf_low precision_conf_high
#>   <chr>   <int>     <int>     <dbl>              <dbl>               <dbl>
#> 1 I           1         1     1                 0.0250               1    
#> 2 P           2         2     0.5               0.0126               0.987
#> 3 C           3         3     0.667             0.0943               0.992
#> # ℹ 3 more variables: recall <dbl>, recall_conf_low <dbl>,
#> #   recall_conf_high <dbl>

# The same metrics from a confusion table.
tab <- confusion(ratings, label_levels = c("I", "P", "C"))
confusion_metrics(tab)
#> # A tibble: 3 × 9
#>   label support predicted precision precision_conf_low precision_conf_high
#>   <chr>   <int>     <int>     <dbl>              <dbl>               <dbl>
#> 1 I           1         1     1                 0.0250               1    
#> 2 P           2         2     0.5               0.0126               0.987
#> 3 C           3         3     0.667             0.0943               0.992
#> # ℹ 3 more variables: recall <dbl>, recall_conf_low <dbl>,
#> #   recall_conf_high <dbl>
```
