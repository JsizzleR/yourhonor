# Calibrate an LLM-as-judge against human labels

Runs a judge on a redacted view of a human-labeled set and reports
agreement, confusion metrics, and (for multiple human raters) human
reliability. The result is descriptive evidence; deployment decisions
require an explicit
[`acceptance_policy()`](https://jsizzler.github.io/yourhonor/reference/acceptance_policy.md).

## Usage

``` r
calibrate_judge(
  gold,
  judge = NULL,
  input_cols = NULL,
  id_col = NULL,
  human_cols = "human_label",
  judge_col = "judge_label",
  label_levels = NULL,
  scale = c("nominal", "ordinal"),
  weights = NULL,
  coefficients = NULL,
  conf_level = 0.95,
  n_boot = 2000,
  seed = NULL,
  provenance = list(),
  audit_id = NULL
)
```

## Arguments

- gold:

  A data frame of labeled examples.

- judge:

  A function receiving the redacted judge input, or `NULL` to read
  precomputed labels from `judge_col`.

- input_cols:

  Columns visible to `judge`. Required when `judge` is a function, and
  refused when `judge = NULL`, where no judge receives them. Human
  columns and `judge_col` are forbidden.

- id_col:

  Optional unique item identifier. A sequential internal ID is generated
  when omitted.

- human_cols:

  Human-rater columns. Supplying more than one enables the
  modal-consensus rule described in the "Human consensus" section and
  the human-reliability benchmark.

- judge_col:

  Column containing precomputed judge labels when `judge` is `NULL`.

- label_levels:

  Complete label universe in intended order. Labels are matched exactly
  (case and whitespace are significant) and empty labels are rejected;
  represent a failed judgement as `NA`. Required for
  `scale = "ordinal"`, where a warning is raised when it orders the
  levels of an ordered factor column (a human column, or `judge_col`
  when `judge = NULL`) neither forwards nor reversed. When `NULL`, the
  universe is inferred from the human and judge labels together: when
  factor columns (or a judge returning a factor) declare at least two
  levels between them, those levels come first, then any other observed
  label; otherwise the observed labels in the order first seen. A label
  only the judge used is then a category, not an error; when
  `label_levels` is supplied, every human and judge label must be in it.

- scale:

  Whether labels are nominal or ordinal.

- weights:

  Agreement weights. `NULL` selects `"unweighted"` for nominal labels
  and `"ordinal"` for ordinal labels. Weighted coefficients use Gwet's
  weight schemes; see
  [`agreement()`](https://jsizzler.github.io/yourhonor/reference/agreement.md).

- coefficients:

  Passed to
  [`agreement()`](https://jsizzler.github.io/yourhonor/reference/agreement.md).
  `NULL` (the default) selects Cohen's kappa, Krippendorff's alpha, and
  Gwet's AC1, with AC2 replacing AC1 for weighted (ordinal) runs.

- conf_level, n_boot, seed:

  Passed to
  [`agreement()`](https://jsizzler.github.io/yourhonor/reference/agreement.md).

- provenance:

  Optional named list describing the judge model, prompt, parameters,
  and other audit metadata.

- audit_id:

  Optional identifier for the frozen judge, prompt, and dataset
  snapshot. Use the same non-empty value for related bias and stability
  evidence so
  [`report()`](https://jsizzler.github.io/yourhonor/reference/report.md)
  can prevent accidental cross-run bundling.

## Value

A `yourhonor_calibration` object with agreement, confusion and per-class
metrics, human reliability, judge-versus-each-rater agreement,
item-level evidence, label metadata, and provenance.

Item-level elements:

- `items`: `gold` plus `.item_id` (source-row key), `.judge_item_id`
  (the shuffled key the judge saw; `NA` when `judge = NULL`), the judge
  label, `agree`, and for several raters `human_consensus`,
  `n_human_observed`, and `consensus_tie`. These names are reserved: a
  `gold` column (or `judge_col`) using one is an error rather than being
  overwritten.

- `judge_output`: the judge's own returned rows, in the order returned
  by the judge, keyed by `.judge_item_id` with `.item_id` joined in and
  `label` as character; `NULL` when `judge = NULL`. The judge must not
  return a `.judge_item_id` column of its own.

- `blinding_map`: one row per item mapping `.judge_item_id` to
  `.item_id`; `NULL` when `judge = NULL`.

Exclusion counts: `n_input` items were supplied, `n_items` had a usable
human consensus and judge label, `n_excluded = n_input - n_items`, and
`n_consensus_ties` and `n_insufficient_human_ratings` break the
exclusions down for multi-rater runs.

Judge abstentions: `n_judge_abstentions` counts items with a human
reference but no judge label, `judge_abstention_rate` divides it by the
items with a human reference, and `judge_abstention_conf_low` and
`judge_abstention_conf_high` are its Clopper-Pearson interval. See the
"Missing labels and abstentions" section.

## The judge contract

When `judge` is supplied, `input_cols` is required. The judge receives
only a generated `.item_id` plus those columns; human labels and
existing judge labels are never exposed. It may return either one label
per input row or a data frame with `.item_id` and `label` columns.
Structured output is preferred because IDs make accidental reordering
detectable.

Items are shuffled before grading and the `.item_id` the judge sees is
assigned after the shuffle, so it is not the source row number. The
result keeps both keys: `items$.item_id` is the source-row audit key and
`items$.judge_item_id` is the key the judge saw. Join judge-side logs
(prompts, raw completions) on `.judge_item_id`, never on `.item_id`.

## Human consensus

With one column in `human_cols` the human label is used as given. With
several, each item's reference label is the modal (most frequent) human
label. An item with fewer than two observed human ratings has no
consensus and is counted in `n_insufficient_human_ratings`; an item
whose most frequent labels tie has no consensus either, is flagged in
`items$consensus_tie`, and is counted in `n_consensus_ties`. Items
without a consensus are excluded from the judge-versus-consensus
agreement and confusion evidence (`n_excluded`). Because ties are
exactly the items humans found hardest, and with two raters every human
disagreement is a tie, the judge is scored only on the items humans
agreed about; that direction is optimistic. `per_rater_agreement`
compares the judge against each rater on every item and is the all-items
counterpart.

## Missing labels and abstentions

Encode a judge abstention, refusal, or unparseable output as `NA` in the
judge label (or return `NA` from `judge`); never as a sentinel label
such as `"abstain"`, which would be scored as a category. Encode a
missing human rating as `NA` in its human column. The two are reported
separately: an item with a human reference and an `NA` judge label is a
judge abstention (`n_judge_abstentions`), while an item without a human
reference is a gap in the gold set. Both are excluded from agreement and
counted in `n_excluded`, which
[`acceptance_policy()`](https://jsizzler.github.io/yourhonor/reference/acceptance_policy.md)'s
`max_missing_rate` bounds; its optional `max_abstention_rate` bounds the
abstention interval alone, because a judge that abstains on hard items
looks more accurate on the items it did answer. A numeric `NaN`, in a
gold column or from `judge`, is read as `NA`, never as the label
`"NaN"`.

## Examples

``` r
gold <- tibble::tibble(
  input = paste("answer", 1:8),
  human_label = c("C", "C", "P", "I", "C", "P", "I", "C"),
  judge_label = c("C", "C", "C", "I", "C", "P", "P", "C")
)
calibrate_judge(gold, label_levels = c("I", "P", "C"),
                n_boot = 200, seed = 1)
#> 
#> ── yourhonor judge calibration ─────────────────────────────────────────────────
#> 8 usable items of 8 - descriptive judge-vs-human agreement:
#> • cohen_kappa: 0.58 [0.07, 1.00] (moderate)
#> • krippendorff_alpha: 0.60 [0.05, 1.00] (moderate)
#> • gwet_ac1: 0.65 [0.25, 1.00] (substantial)
#> 2 disagreements; inspect `confusion_metrics()` before use.
#> ℹ No deployment decision is implied; call report() with a policy to assess one.
```
