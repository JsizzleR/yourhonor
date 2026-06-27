# yourhonor 0.0.0.9000

* First development version.
* `calibrate_judge()` runs an LLM-as-judge over a human-labeled gold set and
  reports judge-vs-human agreement (`agreement()`) with bootstrap confidence
  intervals and a confusion matrix (`confusion()`).
* `probe_bias()` measures paired verbosity, position and self-preference bias.
* `test_retest()` measures grade stability across repeated judge runs.
* `report()` renders a one-paragraph "is my judge usable" verdict.
