# Report judge evidence or assess an explicit policy

Without `policy`, this function renders descriptive evidence and returns
the decision `"NOT_ASSESSED"`. It never infers that missing bias or
stability evidence passed. With an
[`acceptance_policy()`](https://jsizzler.github.io/yourhonor/reference/acceptance_policy.md),
every criterion is rendered as `PASS`, `FAIL`, `NOT_ASSESSED`, or
`INDETERMINATE`.

## Usage

``` r
report(
  x,
  bias = NULL,
  stability = NULL,
  policy = NULL,
  output = c("console", "md")
)
```

## Arguments

- x:

  A `yourhonor_calibration` object.

- bias:

  Optional result from
  [`probe_bias()`](https://jsizzler.github.io/yourhonor/reference/probe_bias.md),
  [`probe_position()`](https://jsizzler.github.io/yourhonor/reference/probe_position.md),
  or
  [`combine_bias()`](https://jsizzler.github.io/yourhonor/reference/combine_bias.md).

- stability:

  Optional result from
  [`test_retest()`](https://jsizzler.github.io/yourhonor/reference/test_retest.md).

- policy:

  Optional policy from
  [`acceptance_policy()`](https://jsizzler.github.io/yourhonor/reference/acceptance_policy.md).

- output:

  Default rendering when the report is printed: `"console"` (styled cli
  output) or `"md"` (Markdown lines).

## Value

A `yourhonor_report` list with elements `decision` (one of `PASS`,
`FAIL`, `INDETERMINATE`, `NOT_ASSESSED`), `checks` (a tibble with one
row per policy check: `check`, `status`, `value`, `op`, `threshold`, and
`reason`), the supplied `calibration`, `bias`, `stability`, and
`policy`, and the default `output`. The report is returned visibly, so
calling `report()` at the console renders it.

## Details

The result is the same object whichever `output` is chosen; `output`
only sets how it prints by default. `print(x, output = "md")` or
`format(x, output = "md")` render the Markdown form of any report, and
[`as.character()`](https://rdrr.io/r/base/character.html) always returns
the Markdown lines.

## Examples

``` r
gold <- tibble::tibble(
  human_label = c("C", "C", "P", "I", "C", "P", "I", "C"),
  judge_label = c("C", "C", "C", "I", "C", "P", "P", "C")
)
cal <- calibrate_judge(gold, label_levels = c("I", "P", "C"),
                       n_boot = 200, seed = 1)
rep <- report(cal, output = "md")
rep$decision
#> [1] "NOT_ASSESSED"
rep
#> ## Judge evidence (n = 8 of 8)
#> 
#> Assessment: **NOT_ASSESSED**.
#> 
#> Agreement with humans: **gwet_ac1 = 0.65** [0.25, 1.00] (substantial).
#> 
#> ### Per-class evidence
#> 
#> - `I`: support 2, precision 1.00, recall 0.50
#> - `P`: support 2, precision 0.50, recall 0.50
#> - `C`: support 4, precision 0.80, recall 1.00
#> 
#> Bias probes: **NOT ASSESSED**.
#> Test-retest stability: **NOT ASSESSED**.
#> 
#> ### Assessment checks
#> 
#> - **NOT_ASSESSED** `acceptance_policy`: No acceptance policy was supplied; evidence is descriptive only.
```
