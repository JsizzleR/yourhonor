# yourhonor: Calibrate Large Language Model Judges

Treat a large language model used as an evaluator (an "LLM-as-judge") as
a measurement instrument and report its psychometric properties. Given a
judge and a set of human-labeled examples, 'yourhonor' reports
chance-corrected agreement between the judge and humans (Cohen's kappa,
Krippendorff's alpha and Gwet's AC1 and weighted AC2) with bootstrap
confidence intervals, human-human reliability when several raters are
supplied, per-class error intervals, structured paired bias probes and
test-retest stability. Explicit acceptance policies turn evidence into
conservative decisions, and versioned artifacts support portable
aggregate audit review. It complements the 'ellmer' and 'vitals'
evaluation stack: it does not run evaluations, it measures the judge
that scores them.

## See also

Useful links:

- <https://github.com/JsizzleR/yourhonor>

- Report bugs at <https://github.com/JsizzleR/yourhonor/issues>

## Author

**Maintainer**: Jay Clark <27960665+JsizzleR@users.noreply.github.com>

Authors:

- Jay Clark <27960665+JsizzleR@users.noreply.github.com>
