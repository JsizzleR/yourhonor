# yourhonor 0.0.0.9000

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
