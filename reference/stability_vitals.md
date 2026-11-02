# Measure stability from precomputed vitals epochs

Converts long-form vitals samples into an item-by-epoch rating matrix
and computes the same stability evidence as
[`test_retest()`](https://jsizzler.github.io/yourhonor/reference/test_retest.md)
without calling a model. Repeated epochs are treated as repeated
measurements of the same item, never as independent calibration rows.

## Usage

``` r
stability_vitals(
  task,
  id_col = "id",
  epoch_col = "epoch",
  score_col = "score",
  content_cols = NULL,
  label_levels = NULL,
  weights = c("unweighted", "linear", "quadratic", "ordinal"),
  conf_level = 0.95,
  n_boot = 1000,
  seed = NULL,
  audit_id = NULL
)
```

## Arguments

- task:

  A `vitals::Task`-like object with a `get_samples()` method, or a
  samples data frame.

- id_col:

  Item identifier shared across epochs.

- epoch_col:

  Epoch/run identifier.

- score_col:

  Precomputed judge-score column.

- content_cols:

  Every column visible to the judge (typically `input`, `target`, and
  `result`). Values must be invariant within item ID; otherwise epochs
  measure changing evaluation outcomes, not judge test-retest
  reliability. Identifier, epoch, and score columns are not content
  columns.

- label_levels:

  Optional complete label universe. For unweighted runs left `NULL` with
  a factor `score_col`, the factor's non-blank levels are used so
  declared-but-unobserved labels still count toward chance agreement.
  Weighted runs always require it: a factor's level order is not taken
  as evidence of the ordinal order.

- weights:

  Agreement weights passed to Gwet's coefficient.

- conf_level:

  Bootstrap confidence level.

- n_boot:

  Number of item-bootstrap resamples.

- seed:

  Optional bootstrap seed.

- audit_id:

  Optional evidence-bundle identifier shared with calibration.

## Value

A `yourhonor_stability` object with `source = "vitals"` provenance.
