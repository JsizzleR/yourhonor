# yourhonor 0.0.0.9000

* Ratings are integer-coded before they reach irrCAC, which upper-cases, trims,
  and blanks character labels internally. Labels that differ only by case or
  whitespace now stay distinct in every coefficient and in test-retest
  stability, matching `confusion()`. Empty or whitespace-only labels are
  rejected with a request to use `NA`.
* `probe_position()` counts an item toward the directional sign test only when
  it was decided in both display orders and its canonical choice moved with
  the order, so abstaining on one order no longer reads as a position
  preference. A tie counts as a decided judgement that commits to neither
  slot. The directional rate and interval now use scored items as the
  denominator, as the pointwise probes do.
* Blind grading strips element names and free-form attributes from every
  judge-visible vector, list element, record field and nested frame, closing
  a channel through which a column built with `sapply()` over labels or IDs
  carried them to the judge row-aligned through the shuffle. Structural
  attributes (class, levels, tzone, units, dim, ptype) are kept, and S4
  objects, environments and functions inside list columns pass through as is.
* Inferred label universes now union declared factor levels with every label
  observed in a non-factor column, so `confusion()` no longer silently drops a
  pair whose label is absent from a factor's levels.
* Calibration now shuffles redacted judge inputs before assigning opaque keys,
  validates keyed output, preserves a declared label universe, and distinguishes
  human reliability from a purported ceiling.
* Agreement supports nominal and ordinal weights, fixed-category bootstraps,
  missingness accounting, and explicit indeterminate states for degenerate data.
* Per-class precision and recall now include exact confidence intervals through
  `confusion_metrics()`.
* `probe_position()` uses neutral display slots, structurally swapped candidates,
  and stable canonical identity. Pointwise probes are blind-keyed in one batch;
  both APIs report raw missingness and conservative directional intervals.
* `test_retest()` reports pairwise disagreement with uncertainty and separates
  it from the run-count-dependent any-flip rate. Single-class runs retain that
  disagreement evidence but report chance-corrected agreement as unidentified.
* `acceptance_policy()` gates calibration, class errors, human-gold reliability,
  probe coverage/effects, and stability with explicit sample-size, missingness,
  and confidence-level requirements. `report()` provides conservative `PASS`,
  `FAIL`, `NOT_ASSESSED`, and `INDETERMINATE` outcomes.
* `audit_vitals()` consumes precomputed vitals scores without another model call.
  `stability_vitals()` reuses repeated scores only after judge-visible content is
  verified invariant and distinct from ID/epoch/score roles, and `write_audit()`
  emits a versioned aggregate summary.
