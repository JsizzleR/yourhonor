# Changelog

## yourhonor 0.1.0

- Initial CRAN submission. yourhonor treats an LLM-as-judge as a
  measurement instrument: it calibrates the judge blind against human
  raters, puts an interval on every estimate, probes it for bias and
  test-retest stability, and lets an explicit acceptance policy decide
  what the evidence supports.

### Calibration and agreement

- [`calibrate_judge()`](https://jsizzler.github.io/yourhonor/reference/calibrate_judge.md)
  compares a judge’s labels with one or more human raters, either from a
  live judge function or from labels already in the data. A live judge
  sees only the columns named in `input_cols`; human columns and the
  judge-label column are refused as inputs. Items are shuffled before
  grading and keyed with a fresh opaque `.item_id`, keyed output is
  validated and restored to source order, and both key spaces are kept
  (`.item_id`, `.judge_item_id` and a `blinding_map`) so judge-side logs
  can be joined back without un-blinding the run.
- With several raters the reference label is the modal consensus (ties
  are excluded and counted), and human-human reliability is reported as
  a benchmark, not a ceiling. Judge abstentions (`NA` labels) are
  reported separately from human gaps, with a Clopper-Pearson interval
  on the abstention rate.
- [`agreement()`](https://jsizzler.github.io/yourhonor/reference/agreement.md)
  reports Cohen’s kappa, Krippendorff’s alpha, and Gwet’s AC1 (nominal)
  or AC2 (weighted) with item-bootstrap intervals over a fixed label
  universe, explicit missingness and an explicit `status` for degenerate
  data. Point estimates come from irrCAC; the bootstrap scores each
  resample natively, with identical draws, in bounded memory.
- Each coefficient also carries `conf_low_exact`, a resampling-free
  finite-sample lower bound on its observed agreement mapped onto the
  coefficient’s scale. A boundary-degenerate bootstrap (a zero-width
  interval at perfect agreement) is flagged unreliable rather than read
  as certainty.
  [`vignette("exact-bound")`](https://jsizzler.github.io/yourhonor/articles/exact-bound.md)
  derives the bound.
- [`confusion()`](https://jsizzler.github.io/yourhonor/reference/confusion.md)
  and
  [`confusion_metrics()`](https://jsizzler.github.io/yourhonor/reference/confusion_metrics.md)
  give the confusion matrix in the declared label order with per-class
  support, precision and recall and exact intervals;
  [`plot_confusion()`](https://jsizzler.github.io/yourhonor/reference/plot_confusion.md)
  draws it with ggplot2.

### Bias probes

- [`probe_bias()`](https://jsizzler.github.io/yourhonor/reference/probe_bias.md)
  runs pointwise verbosity, self-preference and bandwagon probes in one
  blind, randomised batch.
  [`probe_position()`](https://jsizzler.github.io/yourhonor/reference/probe_position.md)
  swaps candidate columns through neutral display slots and maps each
  choice back to the stable candidate identity.
- Every probe reports its flip rate, a net directional rate with a
  conservative interval, a sign-test p-value adjusted across the
  multiple-comparison family, raw missingness and a verdict. A large but
  balanced flip rate is reported but never flagged as bias.
- [`combine_bias()`](https://jsizzler.github.io/yourhonor/reference/combine_bias.md)
  merges probe runs under one multiple-comparison family.

### Stability

- [`test_retest()`](https://jsizzler.github.io/yourhonor/reference/test_retest.md)
  re-grades items and reports mean pairwise disagreement between runs
  with a conservative interval, a chance-corrected stability
  coefficient, and an any-flip rate tied explicitly to the number of
  runs.
- It also reports a worst-case pairwise disagreement in which every pair
  involving a missing re-run judgment disagrees, so abstaining where the
  judge is unstable cannot hide instability, and a best-case
  disagreement in which each missing judgment takes its item’s most
  common label, so abstaining on runs that agree cannot fail a stable
  judge.

### Acceptance policies and reports

- [`acceptance_policy()`](https://jsizzler.github.io/yourhonor/reference/acceptance_policy.md)
  makes every threshold a declared parameter: sample size, class
  support, agreement, per-class recall and precision, missingness and
  abstention, probe and stability requirements. A threshold supplied
  without the gate that reads it is an error.
- [`report()`](https://jsizzler.github.io/yourhonor/reference/report.md)
  turns the evidence into `PASS`, `FAIL`, `NOT_ASSESSED` or
  `INDETERMINATE`, check by check, with numeric `value`, `op` and
  `threshold` columns. Unscored probe items, missing position repeats
  that could change a scored item’s outcome, and missing re-run
  judgments count against the judge in the upper bounds a policy
  assesses and in its favour in the lower bounds (position ties no
  answer could break stay out of them), so missing evidence can keep a
  check from passing or failing but not cause either; the missing-rate
  checks gate missingness itself. With `repeats >= 2`, a scored position
  item whose missing repeats could change its outcome moves only in
  directions those repeats can reach (in the upper bound, either way
  when they could leave it unscored). The policy reads such items from
  the probe’s per-judgment `evidence` attribute, which records the
  `tie_label`, and makes both interval checks `INDETERMINATE` when that
  evidence is needed but absent or does not match the probe’s counts.
  The `min_probe_n` count leaves out each scored position item that a
  completion of its missing repeats could leave unscored (without the
  evidence, every scored item that may have a missing repeat), so a
  `PASS` or `FAIL` never rests on fewer scored items than the policy
  requires; the `bias_<probe>_n` value is that count. A count or missing
  rate that is not a finite number makes its check `INDETERMINATE`, not
  `FAIL`. Without a policy the report is descriptive. It prints to the
  console or renders as Markdown, and its agreement and stability
  headlines show the bounds the policy assesses beside the observed
  interval.
- Every piece of evidence can carry an `audit_id`, and
  [`report()`](https://jsizzler.github.io/yourhonor/reference/report.md)
  refuses to bind evidence tagged for a different judge.

### Audit artifacts and vitals

- [`write_audit()`](https://jsizzler.github.io/yourhonor/reference/write_audit.md)
  writes a Markdown report or a versioned, language-neutral JSON summary
  (`yourhonor.audit.v1`) that omits item rows. It carries the stability
  worst- and best-case bounds and each bias probe’s `n_unscored_tied`.
  [`?write_audit`](https://jsizzler.github.io/yourhonor/reference/write_audit.md)
  documents every field, and says when a bias check’s value can be
  recomputed from the JSON alone: not an interval value that counts
  scored position items whose missing repeats could change their
  outcome, which rests on per-judgment evidence the JSON leaves out. A
  field’s JSON number type never changes between runs: counts are
  integers and every other number is written as a decimal.
- [`audit_vitals()`](https://jsizzler.github.io/yourhonor/reference/audit_vitals.md)
  and
  [`stability_vitals()`](https://jsizzler.github.io/yourhonor/reference/stability_vitals.md)
  reuse grades and epochs already present in a vitals task without
  another model call;
  [`stability_vitals()`](https://jsizzler.github.io/yourhonor/reference/stability_vitals.md)
  refuses epochs whose judge-visible content changed.
- [`vignette("yourhonor")`](https://jsizzler.github.io/yourhonor/articles/yourhonor.md)
  walks through an audit end to end, with further vignettes on bias
  probes, stability and vitals, and policies and audit artifacts; an
  article on the package website shows how to wrap an ellmer chat as a
  judge.

### Errors and missing values

- Errors carry classes by family (`yourhonor_input_error`,
  `yourhonor_judge_error`, `yourhonor_policy_error`,
  `yourhonor_binding_error`), each also of class `yourhonor_error`, so a
  caller can catch a family without matching message text (see
  `?yourhonor-errors`).
- A missing label is `NA` everywhere: a numeric `NaN` from a judge, a
  precomputed rating column or a vitals score is missing, never the
  label `"NaN"`, and empty or whitespace-only labels are refused.

### Data

- `support_gold` is an eight-item synthetic dataset for API
  demonstration.
- `synthetic_gold` is a 200-item synthetic dataset with three simulated
  raters, sized so an audit can reach a verdict. Every label is planted
  and the text is templated; it is not a benchmark.
