# yourhonor: Calibrate Large Language Model Judges

Treat a large language model used as an evaluator (an "LLM-as-judge") as
a measurement instrument and report its psychometric properties. Given a
judge and a set of human-labeled examples, 'yourhonor' reports
chance-corrected agreement between the judge and humans with bootstrap
confidence intervals: Cohen's kappa (Cohen 1960)
[doi:10.1177/001316446002000104](https://doi.org/10.1177/001316446002000104)
, Krippendorff's alpha (Hayes and Krippendorff 2007)
[doi:10.1080/19312450709336664](https://doi.org/10.1080/19312450709336664)
, and Gwet's AC1 (Gwet 2008)
[doi:10.1348/000711006X126600](https://doi.org/10.1348/000711006X126600)
and weighted AC2 (Gwet 2014, ISBN:978-0970806284). It also reports
human-human reliability when several raters are supplied, per-class
error intervals, structured paired bias probes and test-retest
stability. Explicit acceptance policies turn evidence into conservative
decisions, and versioned artifacts support portable aggregate audit
review. It complements the 'ellmer' and 'vitals' evaluation stack: it
does not run evaluations, it measures the judge that scores them.

## See also

Useful links:

- <https://github.com/JsizzleR/yourhonor>

- <https://jsizzler.github.io/yourhonor/>

- Report bugs at <https://github.com/JsizzleR/yourhonor/issues>

## Author

**Maintainer**: Jay Clark <27960665+JsizzleR@users.noreply.github.com>

Authors:

- Jay Clark <27960665+JsizzleR@users.noreply.github.com>
