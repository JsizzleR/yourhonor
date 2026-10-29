# yourhonor 0.0.0.9000 (development version, unreleased)

Initial development version. Nothing has been released; the list below is the
feature set as it stands, and the "Review fixes" section records what changed
during the pre-release code review so that early adopters can update their
calls.

## Features

* `calibrate_judge()` compares judge labels with one or more human raters.
  A live judge sees only the columns listed in `input_cols`; human columns and
  `judge_col` are rejected as inputs. Items are shuffled before grading and
  keyed with a fresh opaque `.item_id`; keyed output is validated and restored
  to source order. With several raters the reference label is the modal
  consensus, ties are excluded and counted, and human-human reliability is
  reported as a benchmark rather than a ceiling.
* `agreement()` reports Cohen's kappa, Krippendorff's alpha, and Gwet's AC1
  (nominal) or AC2 (weighted) with item-bootstrap intervals, a fixed label
  universe, explicit missingness, and an explicit `status` for degenerate
  data. Zero-width bootstraps at empirically perfect agreement are flagged
  unreliable rather than reported as certainty.
* `confusion()` and `confusion_metrics()` give the confusion matrix in declared
  label order plus per-class support, precision, and recall with exact
  intervals; `plot_confusion()` draws the matrix with ggplot2.
* `probe_bias()` runs pointwise verbosity, self-preference, and bandwagon
  probes in one blind, randomised batch. `probe_position()` swaps structured
  candidate columns into neutral display slots and maps choices back to stable
  candidate identity. `combine_bias()` merges probe evidence under one
  multiple-comparison family. All three report raw missingness and
  conservative directional intervals.
* `test_retest()` re-grades items and reports mean pairwise disagreement with
  a hybrid bootstrap/Wilson interval, chance-corrected stability, and an
  any-flip rate that is explicitly tied to the number of runs.
* `acceptance_policy()` makes thresholds explicit, including sample-size,
  class-support, missingness, probe, and stability requirements. `report()`
  turns the evidence into `PASS`, `FAIL`, `NOT_ASSESSED`, or `INDETERMINATE`
  and is descriptive only when no policy is supplied.
* `audit_vitals()` and `stability_vitals()` reuse grades and epochs already
  present in a vitals task without another model call; `stability_vitals()`
  refuses epochs whose judge-visible content changed. `write_audit()` writes a
  Markdown report or a versioned JSON summary (`yourhonor.audit.v1`) that
  omits item rows.
* `support_gold` is an eight-item synthetic dataset for API demonstration.

## Review fixes

User-visible changes made while addressing the pre-release code review.

### Calibration edge cases (#1)

* A calibration whose human (consensus) reference uses a single category now
  reports `status = "insufficient_reference_variation"` for every
  coefficient instead of an estimate: Gwet's AC1 read "almost perfect" there
  while kappa read zero.
* Agreement tables gain `conf_low_exact`, a Clopper-Pearson lower bound on
  observed agreement mapped onto the coefficient's scale. Policies assess
  the smaller of it and the bootstrap `conf_low` on every sample; on a
  perfect sample the bootstrap interval is a zero-width point at 1, so the
  exact bound decides there, and a perfect sample is no longer
  `INDETERMINATE` where one error could `PASS`. Small-`n` bootstrap optimism
  no longer reaches a decision. For Krippendorff's alpha with missing
  ratings, which weights items by their number of ratings, the bound is
  scaled by the fewest ratings on an item over the mean, so it can no
  longer exceed the estimate. For
  weighted coefficients the exact bound counts only exact agreement, so
  ordinal policies whose disagreements are mostly adjacent pass less often.
  For kappa and alpha on imbalanced labels the bound is weak (it is divided
  by `1 - pe`) and usually decides the check below a few hundred items.
* Chance agreement now counts only the categories some rater used, so
  declaring an unused level no longer moves Gwet's AC1 (it did not move
  kappa or alpha) and no longer enters AC2's chance term. Weights still
  follow the full declared scale, so for AC2 an unused level still changes
  the distances it spans. This applies to `test_retest()` too.
* `calibrate_judge()` reports judge abstentions (an `NA` judge label on an
  item with a human reference) separately from human gaps:
  `n_judge_abstentions`, `judge_abstention_rate`, and its Clopper-Pearson
  interval. `acceptance_policy(max_abstention_rate = )` bounds the
  interval's upper end. The encoding is documented under "Missing labels and
  abstentions" in `?calibrate_judge`.
* `acceptance_policy()`'s `coefficient` now defaults to `NULL`, the
  calibration's Gwet coefficient (AC1 nominal, AC2 ordinal); the old
  `"gwet_ac1"` default left an ordinal calibration `NOT_ASSESSED`. When the
  calibration did not compute that Gwet coefficient (for example
  `coefficients = "cohen_kappa"`), the agreement check is `NOT_ASSESSED`
  and the rest of the report is still assessed. Naming a coefficient the
  calibration did not compute is an error at assessment.
* The JSON artifact adds `calibration.confusion` (long-form counts),
  `calibration.per_rater_agreement`, and the abstention fields.

### Labels and blinding

* Ratings are integer-coded before they reach irrCAC, which upper-cases, trims,
  and blanks character labels internally. Labels that differ only by case or
  whitespace now stay distinct in every coefficient and in test-retest
  stability, matching `confusion()`. Empty or whitespace-only labels are
  rejected everywhere with a request to use `NA` instead.
* Inferred label universes union declared factor levels with every label
  observed in a non-factor column, so `confusion()` no longer silently drops a
  pair whose label is absent from a factor's levels.
* Blind grading strips element names and free-form attributes from every
  judge-visible vector, list element, record field, and nested frame, closing a
  channel through which a column built with `sapply()` over labels or IDs
  carried them to the judge row-aligned through the shuffle. Structural
  attributes (class, levels, tzone, units, dim, ptype) are kept; S4 objects,
  environments, and functions inside list columns pass through as is.
* Calibration results keep both key spaces: `items$.item_id` is the source-row
  key and `items$.judge_item_id` is the key the judge saw. `cal$blinding_map`
  maps between them and `cal$judge_output` is keyed by both. `test_retest()`
  returns a parallel `judge_item_ids` matrix. Join judge-side logs on the judge
  key, never on the row number.
* A judge that returns a factor declares its own label universe; those levels
  are kept when `label_levels` is `NULL`. `stability_vitals()` likewise uses a
  factor `score_col`'s levels so declared-but-unobserved labels count toward
  chance agreement.

### Agreement and confusion

* `agreement()` and `confusion()` gain explicit column arguments
  (`rater_cols`; `human_col` and `judge_col`). Without them, columns named
  `human` and `judge` are used when both exist; otherwise `agreement()` treats
  every column as a rater and `confusion()` reads a two-column frame
  positionally and errors on wider frames. Any inferred choice that ignored or
  positionally read columns is reported with a message of class
  `yourhonor_inferred_columns`. `plot_confusion()` forwards `...` to
  `confusion()`.
* `coefficients` now defaults to `NULL` in `agreement()` and
  `calibrate_judge()`, meaning kappa, alpha, and AC1 with AC2 substituted for
  AC1 under weights. Explicitly requesting `"gwet_ac1"` with weights is an
  error rather than a silent substitution.
* `agreement(<calibration>)` inherits every stored setting (`coefficients`,
  `conf_level`, `n_boot`, `seed`, `label_levels`, `weights`) for arguments left
  at their default, so `agreement(cal, n_boot = 50)` keeps the stored seed and
  confidence level. One set of bootstrap resamples is shared by all
  coefficients within a call.
* `plot_confusion()` keeps the declared label order on both axes instead of
  sorting alphabetically; `confusion()` attaches the resolved order as the
  `"label_levels"` attribute.
* `print()` for a calibration shows each coefficient's interval and
  interpretation; the interval was previously dropped.
* The modal-consensus rule for multiple raters, its exclusions
  (`n_consensus_ties`, `n_insufficient_human_ratings`, `n_excluded`), and its
  optimistic direction are documented in `?calibrate_judge`.

### Bias probes

* `probe_bias()` and `probe_position()` return one shared schema (32 columns)
  built by a single constructor, so pointwise and position rows can always be
  combined. `n_missing` and `pair_missing_rate` are renamed `n_unscored` and
  `unscored_rate`; `n_items_missing` (items with at least one `NA` judgment)
  and `n_directional_items` are reported for every probe. The `evidence` and
  `item_evidence` attributes are lists named by probe on every object;
  `combine_bias()` concatenates them without nesting.
* `probe_position()` counts an item toward the directional sign test only when
  it was decided in both display orders and its canonical choice moved with
  the order, so abstaining on one order no longer reads as a position
  preference. A tie counts as a decided judgement that commits to neither
  slot. The directional rate and interval use scored items as the denominator,
  as the pointwise probes do.
* A probe with no label movement now enters the multiple-comparison family
  with `p_value = 1` instead of `NA`; `p_value` is `NA` only when
  `status == "insufficient_data"`, and `combine_bias()` recomputes verdicts.
* `flip_threshold` is now a practical-significance floor on the net
  `directional_rate`, not on the any-direction `flip_rate`: a large but
  balanced flip rate is reported but never flagged `"BIAS"`.
* The directional interval's upper bound is capped at the flip-rate upper bound
  over the same denominator, since the net rate can never exceed the
  any-direction rate.
* `probe_bias()` aborts when `input` is not a character column or contains
  `NA`; a pasted `NA` would otherwise be graded as a real answer and hide
  abstention from the missingness gate.

### Stability

* The pairwise-disagreement interval is documented as a hybrid: the percentile
  item bootstrap widened to a Wilson score interval for the mean per-item
  disagreement. It can only widen. The chance-corrected estimate and its
  `stability_status` are derived from the same filtered matrix, and the label
  universe for the coefficient comes from every run.

### Policy and report

* `report()` always returns a visible `yourhonor_report` list with `decision`,
  `checks`, and the supplied evidence; `output` only sets the default print
  rendering. Use `x$decision` instead of `attr(x, "decision")`, and
  `format(x, output = "md")` or `as.character(x)` for the Markdown lines.
* `checks` gains numeric `value` and `threshold` columns and an `op` column
  (`">="`, `"<="`, `"lower >="`, `"upper <="`, `"="`); the old prose
  `threshold` string is gone. Each required probe yields up to four checks
  (`bias_<probe>`, `_direction`, `_missing_rate`, `_n`), and printed check lines
  show `value (op threshold)`.
* `acceptance_policy()` rejects a stability or probe threshold supplied
  without its gate (`max_pairwise_disagreement` or `required_probes`), so a
  policy can never carry a threshold the assessment does not read. Those
  errors and probe-name errors carry class `yourhonor_policy_error`. Every
  required argument is checked with `rlang::check_required()`.
* Validation errors name the caller's argument (for example `n_boot`,
  `ordered_levels`, `choice_levels`) and the calling function rather than an
  internal helper.
* The report's bias summary distinguishes required probes from probes supplied
  outside the policy, and mentions descriptive signals in the latter.

### vitals adapters and artifacts

* `audit_vitals()` uses `score_col` directly as the judge-label column, so
  `cal$judge_col`, `cal$items`, and the JSON provenance no longer contain an
  internal sentinel name. It aborts when the samples already contain a
  `human_col` column instead of overwriting it, and validates `provenance`.
* `write_audit()` gains `overwrite = FALSE`; an existing file is an error unless
  `overwrite = TRUE`. JSON is written at full precision (`digits = NA`), every
  absent value is `null` (never `{}`), vector-by-contract fields
  (`label_levels`, `coefficients`, `required_probes`, adapter `input_cols` and
  `human_cols`) are always arrays, class-specific `min_recall` and
  `min_precision` are objects keyed by class, `created_at` is RFC 3339 UTC
  (`2026-01-31T12:00:00Z`), and `checks` carries `value`, `op`, and
  `threshold`. The schema is documented in `?write_audit`.

### Second review pass

* Validation messages that name unknown labels (`agreement()`, `confusion()`,
  `calibrate_judge()`, `test_retest()`, `stability_vitals()`) render again
  instead of failing inside cli; absent explicit columns are pluralised, and
  the label-universe minimum names the actual count (one for agreement and
  confusion, two for calibration).
* Printed check lines raise their precision until the value and threshold
  differ, so a `FAIL` at `0.1005` against `0.1` no longer reads
  `= 0.1 (<= 0.1)`. `checks$value` is always the quantity `op` compares and
  is `NA` when the interval is untrusted; the reason then names the cause
  (a boundary-degenerate bootstrap, too few finite resamples, an unavailable
  or reversed interval) rather than echoing `status = "ok"`.
* A required probe smaller than `min_probe_n` is again `INDETERMINATE` under
  `bias_<probe>` with its interval unassessed, rather than a possible `FAIL`.
  A probe or calibration at another confidence level withholds only its
  interval checks (`bias_<probe>_missing_rate` and `_n`, `usable_items`,
  `calibration_missing_rate`, and `class_support` are still assessed). A
  duplicated coefficient row or an empty class-metrics table is reported as
  `INDETERMINATE` or `NOT_ASSESSED` instead of being read silently.
* `acceptance_policy()` rejects an orphan threshold with a readable sentence,
  and every validation error it raises carries class
  `yourhonor_policy_error`.
* The console report prints every class line and the `n of N` judge-evidence
  line that the Markdown report already had (and, like the Markdown report,
  tolerates a calibration without class metrics), and a non-required probe
  whose verdict is `INCONCLUSIVE` is mentioned in the bias summary whether or
  not the policy names required probes. A check value and threshold that
  only part beyond 15 significant digits are printed exactly.
* `agreement(<calibration>, weights = ...)` re-resolves the inherited
  coefficients under the new weights instead of treating the stored Gwet
  variant as an explicit, conflicting request. `confusion()` and
  `confusion_metrics()` reject `human_col`/`judge_col` on a calibration
  object as `agreement()` rejects `rater_cols`; `confusion_metrics()` applies
  `label_levels` to a calibration's or confusion tibble's stored metrics;
  frames with duplicated column names are refused; the positional-inference
  hint names both selectors; and errors from `confusion_metrics()` name it
  and say whether a calibration object or a confusion tibble was passed.
  A count above the integer range (for example `n_boot = 1e10`) is rejected
  with a message naming that bound.
* The bootstrap draws one resample per iteration, shared by every requested
  coefficient, instead of materialising an `n x n_boot` index matrix, and
  draws nothing when no coefficient is estimable.
* `combine_bias()` carries over only evidence for probes present in each
  constituent's rows, so stale evidence left by row-subsetting cannot shadow
  a newer run, refuses an unnamed evidence list instead of emptying it, and
  names given to `...` no longer prefix the evidence names.
  `probe_position()` reserves `.judge_item_id`, `display_choice`, and
  `canonical_choice`; `probe_bias()` validates `id_col` on the full data
  before sampling.
* Structured judge output returning a `.judge_item_id` column is rejected,
  and `judge_output$label` is always character. `test_retest()` unions a
  judge's factor levels with every label observed across runs, and an unused
  blank factor level is dropped from inferred levels (`test_retest()`,
  `stability_vitals()`). `stability_vitals()` documents that level inference
  applies to unweighted runs only.
* `write_audit()` writes doubles with 17 significant digits so they round-trip
  exactly, refuses a directory as `path`, and the calibration provenance uses
  the same RFC 3339 `created_at` as the top level with `NULL` adapter fields
  (`id_col`, `audit_id`) present as `null`. `provenance` must have unique,
  non-blank names. Any task whose `$get_samples` is callable is accepted by
  the vitals adapters.
