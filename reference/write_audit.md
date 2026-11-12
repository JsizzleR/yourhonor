# Write a portable judge audit artifact

Writes either the Markdown evidence report or a versioned JSON summary.
The JSON artifact does not automatically serialize `x$items`; it
contains aggregate evidence, policy checks, and provenance.

## Usage

``` r
write_audit(
  x,
  path,
  bias = NULL,
  stability = NULL,
  policy = NULL,
  format = c("json", "md"),
  overwrite = FALSE
)
```

## Arguments

- x:

  A `yourhonor_calibration` object.

- path:

  Output file path. An existing file is never replaced unless
  `overwrite = TRUE`, so a re-run cannot silently clobber an earlier
  audit.

- bias:

  Optional bias evidence.

- stability:

  Optional stability evidence.

- policy:

  Optional acceptance policy.

- format:

  `"json"` or `"md"`.

- overwrite:

  Replace an existing file at `path`? Defaults to `FALSE`.

## Value

`path`, invisibly.

## Privacy

Calibration item rows are omitted from JSON. Caller-supplied
`provenance` is serialized verbatim, however, so do not place secrets or
raw item content in provenance fields.

## JSON schema (`yourhonor.audit.v1`)

The JSON artifact is written at full numeric precision: doubles carry 17
significant digits, so every value round-trips exactly (a reader may see
`0.94999999999999996` for `0.95`). That is the precision of the stored
doubles, not of the estimates: irrCAC rounds each agreement and
stability coefficient to five decimals, so every `estimate` (and
stability's `gwet_agreement`) and every bootstrap draw sits on that
grid, and the bootstrap bounds interpolated between draws are no more
precise. `conf_low_exact` is computed from irrCAC's unrounded observed
and chance agreement and is capped at the rounded `estimate` (and at the
bound of the perfect samples it can be corrected into; see
[`agreement()`](https://jsizzler.github.io/yourhonor/reference/agreement.md)).
Every absent value is `null`: an `NA` and an R `NULL` are
indistinguishable to a reader, but neither ever appears as
[`{}`](https://rdrr.io/r/base/Paren.html) or is dropped. Fields that are
vectors by contract are always JSON arrays, even when they hold one
value. Each numeric field outside caller-supplied `provenance` (whose
values are written as the caller typed them) keeps one JSON number type
from run to run: `seed` and the counts (`n`, every `n_*` and
`*_n_boot_effective` field, `support`, `predicted`, `statistic`, and the
policy's `min_n`, `min_class_n`, `min_stability_n`, `min_stability_runs`
and `min_probe_n`) are integers, and every other number, including each
check's `value` and `threshold`, is written with a decimal point or an
exponent (`0.0`, `1.0`), even when it is whole. Top-level members:

- `schema_version`: `"yourhonor.audit.v1"`.

- `created_at`: UTC timestamp in RFC 3339 form, e.g.
  `"2026-01-31T12:00:00Z"`.

- `decision`: the overall assessment (`PASS`, `FAIL`, `INDETERMINATE`,
  or `NOT_ASSESSED`).

- `audit_id`: string or `null`.

- `policy`: `null`, or an object with one member per
  [`acceptance_policy()`](https://jsizzler.github.io/yourhonor/reference/acceptance_policy.md)
  argument. A `null` `coefficient` means the calibration's Gwet
  coefficient, which the `agreement_<coefficient>` check names.
  `required_probes` is an array; `min_recall` and `min_precision` are a
  number when global or an object keyed by class name when
  class-specific; unset thresholds are `null`.

- `checks`: array of objects, one per policy check, with `check`,
  `status`, `value` (the number compared with `threshold`, `null` when
  unavailable: the observed value, which for `bias_<probe>_n` is a count
  of scored items no completion of missing repeats could leave unscored,
  see `min_probe_n` in
  [`acceptance_policy()`](https://jsizzler.github.io/yourhonor/reference/acceptance_policy.md);
  or for an interval check the bound assessed, which for a bias probe's
  flip-rate and directional checks counts every unscored item, and every
  scored position item whose missing repeats could change its outcome,
  against the probe over `n_input`: as a flip for the flip rate, towards
  the worse direction for the directional rate (a scored position item
  only in a direction its repeats can reach, or either way when they
  could leave it unscored), and for the stability disagreement check is
  `stability.pairwise_disagreement_worst_case_high`, which counts every
  pair involving a missing judgment as a disagreement over all
  `n_items`, so it can exceed
  `stability.pairwise_disagreement_conf_high`; only when that worst-case
  bound is unavailable (not a number from 0 to 1, or below the observed
  upper unless the evidence shows nothing missing) does a stability
  `FAIL` report the observed upper), `op` (the comparison the check
  encodes: `">="`, `"<="`, `"lower >="`, `"upper <="`, or `"="`), a
  numeric `threshold`, and `reason`. Without a policy the single
  placeholder check has `null` `op` and `threshold`.

- `calibration`: `n_input`, `n_items`, `n_excluded`, `n_consensus_ties`,
  `n_insufficient_human_ratings`, `n_judge_abstentions`,
  `judge_abstention_rate`, `judge_abstention_conf_low`,
  `judge_abstention_conf_high`, `scale`, `weights`, `conf_level`,
  `n_boot`, `seed` (scalars); `label_levels` and `coefficients`
  (arrays); `agreement`, `confusion`, `confusion_metrics`,
  `human_reliability`, and `per_rater_agreement` (arrays of row objects
  mirroring the tibble columns; `human_reliability` and
  `per_rater_agreement` are `null` for a single rater). `confusion`
  holds one row per reference-by-judge cell, empty cells included, with
  the reference label, the judge label, the count `n`, and its share
  `prop`, so a reader can rebuild the matrix in `label_levels` order.

- `bias`: `null`, or `audit_id` plus `results`, an array of row objects
  with the columns of the bias tibble (see
  [`probe_bias()`](https://jsizzler.github.io/yourhonor/reference/probe_bias.md)),
  among them `n_unscored_tied`, an additive column of
  `yourhonor.audit.v1`. The probes' per-judgment evidence is not
  written, so where a bias check's `reason` counts scored items whose
  missing repeats could change their outcome, or could leave them
  unscored, that count comes from the evidence (without it, the
  `bias_<probe>_n` count is `n_scored` less
  `n_items_missing - (n_unscored - n_unscored_tied)`, taken from 0 to
  `n_scored`, all written). A `bias_<probe>_n` value is then `n_scored`
  less the count in its reason, but an interval check's value also
  depends on how many of those items flipped and which directions each
  could reach, which the reason does not say, so it cannot be recomputed
  from the JSON. Every other bias check's value can be, from the row's
  written columns (see `min_probe_n` in
  [`acceptance_policy()`](https://jsizzler.github.io/yourhonor/reference/acceptance_policy.md)).

- `stability`: `null`, or the summary fields of a `yourhonor_stability`
  object (see
  [`test_retest()`](https://jsizzler.github.io/yourhonor/reference/test_retest.md)):
  `audit_id` (string or `null`), `n_items`, `n_items_scored`, `n_runs`,
  `label_levels` (array), `weights`, `conf_level`, `n_boot`,
  `pairwise_n_boot_effective`, `stability_n_boot_effective`,
  `n_boot_effective` (the smaller of the two), `pairwise_disagreement`
  with its observed `pairwise_disagreement_conf_low` and
  `pairwise_disagreement_conf_high`, `percent_agreement`
  (`1 - pairwise_disagreement`, a proportion despite its name),
  `any_flip_rate_at_n_runs`, the bounds for missing judgments (the
  policy assesses `pairwise_disagreement_worst_case_high` and
  `pairwise_disagreement_best_case_low`; `n_items_complete`, the items
  with every run observed; `pairwise_disagreement_worst_case` and its
  upper bound `pairwise_disagreement_worst_case_high`;
  `any_flip_rate_worst_case`; `pairwise_disagreement_best_case` and its
  lower bound `pairwise_disagreement_best_case_low`; see
  [`test_retest()`](https://jsizzler.github.io/yourhonor/reference/test_retest.md);
  additive members of `yourhonor.audit.v1`), the chance-corrected
  agreement as `gwet_agreement`, `gwet_coefficient`, `gwet_conf_low`,
  `gwet_conf_high`, `gwet_status` and `gwet_boundary_degenerate` (the
  object's `stability`, `stability_coefficient`, `stability_conf_low`,
  `stability_conf_high`, `stability_status` and
  `stability_boundary_degenerate`), `missing_rate`,
  `pairwise_ci_reliable`, `stability_ci_reliable`, `ci_reliable` (both
  intervals reliable), and `provenance`.

- `provenance`: the calibration provenance. The keys `input_cols`,
  `human_cols`, `label_levels`, and `content_cols` are always arrays,
  even when they were supplied by the caller and hold one value;
  `created_at` is RFC 3339 UTC like the top-level field, and adapter
  fields left `NULL` (`id_col`, `audit_id`) are present as `null`.

## Examples

``` r
cal <- calibrate_judge(support_gold, label_levels = c("I", "P", "C"),
                       n_boot = 200, seed = 1)
path <- tempfile(fileext = ".json")
write_audit(cal, path)
audit <- jsonlite::read_json(path)
audit$schema_version
#> [1] "yourhonor.audit.v1"
audit$decision
#> [1] "NOT_ASSESSED"

md <- tempfile(fileext = ".md")
write_audit(cal, md, format = "md")
unlink(c(path, md))
```
