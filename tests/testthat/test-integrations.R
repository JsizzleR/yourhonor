vitals_samples <- function() {
  tibble::tibble(
    id = c("a", "b", "c", "d", "e", "f"),
    input = paste("secret input", 1:6),
    score = c("I", "P", "C", "C", "P", "I")
  )
}

test_that("vitals samples can be audited without another model call", {
  cal <- audit_vitals(
    vitals_samples(),
    human_labels = c("I", "P", "C", "C", "I", "I"),
    label_levels = c("I", "P", "C"),
    coefficients = "gwet_ac1", n_boot = 50, seed = 1,
    provenance = list(model = "example-judge")
  )
  expect_s3_class(cal, "yourhonor_calibration")
  expect_equal(cal$n_items, 6)
  expect_identical(cal$provenance$source, "vitals")
  expect_identical(cal$provenance$score_col, "score")
  expect_identical(cal$provenance$model, "example-judge")
})

test_that("keyed human labels are joined by exact sample ID", {
  labels <- tibble::tibble(
    id = rev(vitals_samples()$id),
    gold = rev(c("I", "P", "C", "C", "I", "I"))
  )
  cal <- audit_vitals(
    vitals_samples(), labels, human_col = "gold",
    label_levels = c("I", "P", "C"), coefficients = "gwet_ac1",
    n_boot = 50, seed = 1
  )
  expect_identical(cal$ratings$human, c("I", "P", "C", "C", "I", "I"))
})

test_that("vitals adapter rejects missing, extra, and duplicate IDs", {
  labels <- tibble::tibble(
    id = c("a", "b", "c", "d", "e", "extra"),
    human_label = c("I", "P", "C", "C", "I", "I")
  )
  expect_error(
    audit_vitals(vitals_samples(), labels, label_levels = c("I", "P", "C")),
    class = "yourhonor_input_error"
  )
  labels$id[[2]] <- labels$id[[1]]
  expect_error(
    audit_vitals(vitals_samples(), labels, label_levels = c("I", "P", "C")),
    class = "yourhonor_input_error"
  )
  expect_error(
    audit_vitals(
      vitals_samples(), human_labels = vitals_samples()$score,
      id_col = c("id", "input"),
      label_levels = c("I", "P", "C")
    ),
    class = "yourhonor_input_error"
  )
})

test_that("multi-epoch vitals samples are not treated as independent items", {
  samples <- vitals_samples()[rep(seq_len(6), each = 2), ]
  samples$epoch <- rep(1:2, times = 6)
  expect_error(
    audit_vitals(
      samples, human_labels = rep(c("I", "P", "C", "C", "I", "I"), each = 2),
      label_levels = c("I", "P", "C")
    ),
    "Multi-epoch", class = "yourhonor_input_error"
  )
})

test_that("precomputed vitals epochs become stability evidence without grading", {
  samples <- vitals_samples()[rep(seq_len(6), each = 3), ]
  samples$epoch <- rep(1:3, times = 6)
  samples$score <- rep(vitals_samples()$score, each = 3)
  samples$score[samples$id == "b" & samples$epoch == 3] <- "C"
  st <- stability_vitals(
    samples, content_cols = "input", label_levels = c("I", "P", "C"),
    n_boot = 50, seed = 1,
    audit_id = "bundle-a"
  )
  expect_s3_class(st, "yourhonor_stability")
  expect_equal(st$n_items, 6)
  expect_equal(st$n_runs, 3)
  expect_gt(st$pairwise_disagreement, 0)
  expect_identical(st$provenance$source, "vitals")
  expect_identical(st$audit_id, "bundle-a")
})

test_that("vitals stability validates epoch keys and column roles", {
  samples <- vitals_samples()[rep(seq_len(6), each = 2), ]
  samples$epoch <- rep(1:2, times = 6)
  expect_error(
    stability_vitals(
      samples, id_col = "id", epoch_col = "id", content_cols = "input"
    ),
    class = "yourhonor_input_error"
  )
  expect_error(
    stability_vitals(samples, content_cols = "id"),
    class = "yourhonor_input_error"
  )
  expect_error(
    stability_vitals(samples, content_cols = "epoch"),
    class = "yourhonor_input_error"
  )
  expect_error(
    stability_vitals(samples, content_cols = "score"),
    class = "yourhonor_input_error"
  )
  duplicated <- vctrs::vec_rbind(samples, samples[1, ])
  expect_error(
    stability_vitals(duplicated, content_cols = "input"),
    class = "yourhonor_input_error"
  )
  expect_error(
    stability_vitals(vitals_samples(), content_cols = "input"), "epoch",
    class = "yourhonor_input_error"
  )
  changed <- samples
  changed$input[changed$id == "a" & changed$epoch == 2] <- "different result"
  expect_error(
    stability_vitals(changed, content_cols = "input"),
    "end-to-end evaluation changes", class = "yourhonor_input_error"
  )
})

test_that("vitals label, score, and ID columns must be distinct", {
  samples <- vitals_samples()
  samples$score <- rev(samples$score)
  expect_error(
    audit_vitals(
      samples,
      human_labels = c("I", "P", "C", "C", "P", "I"),
      human_col = "score",
      label_levels = c("I", "P", "C")
    ),
    class = "yourhonor_input_error"
  )
})

test_that("audit artifacts omit raw item text", {
  cal <- audit_vitals(
    vitals_samples(),
    human_labels = c("I", "P", "C", "C", "I", "I"),
    label_levels = c("I", "P", "C"), coefficients = "gwet_ac1",
    n_boot = 50, seed = 1
  )
  json <- withr::local_tempfile(fileext = ".json")
  md <- withr::local_tempfile(fileext = ".md")
  expect_identical(write_audit(cal, json, format = "json"), json)
  expect_identical(write_audit(cal, md, format = "md"), md)
  json_text <- paste(readLines(json, warn = FALSE), collapse = "\n")
  md_text <- paste(readLines(md, warn = FALSE), collapse = "\n")
  expect_match(json_text, "yourhonor.audit.v1")
  expect_match(json_text, "NOT_ASSESSED")
  expect_false(grepl("secret input", json_text, fixed = TRUE))
  expect_match(md_text, "Judge evidence")
})

test_that("JSON artifacts reject unbound secondary evidence", {
  cal <- audit_vitals(
    vitals_samples(),
    human_labels = c("I", "P", "C", "C", "I", "I"),
    label_levels = c("I", "P", "C"), coefficients = "gwet_ac1",
    n_boot = 50, seed = 1, audit_id = "bundle-a"
  )
  bias <- probe_bias(
    tibble::tibble(input = paste("answer", 1:20)),
    function(d) rep("C", nrow(d)), probes = "verbosity", seed = 1
  )
  path <- withr::local_tempfile(fileext = ".json")
  expect_error(
    write_audit(cal, path, bias = bias, format = "json"),
    "UNBOUND", class = "yourhonor_binding_error"
  )
})

test_that("JSON artifacts preserve an unidentified stability coefficient", {
  cal <- audit_vitals(
    vitals_samples(),
    human_labels = c("I", "P", "C", "C", "I", "I"),
    label_levels = c("I", "P", "C"), coefficients = "gwet_ac1",
    n_boot = 50, seed = 1, audit_id = "bundle-a"
  )
  samples <- vitals_samples()[rep(seq_len(6), each = 3), ]
  samples$epoch <- rep(1:3, times = 6)
  samples$score <- "C"
  stability <- stability_vitals(
    samples, content_cols = "input", label_levels = c("I", "P", "C"),
    n_boot = 50, seed = 1, audit_id = "bundle-a"
  )
  path <- withr::local_tempfile(fileext = ".json")
  write_audit(cal, path, stability = stability, format = "json")
  json_text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(json_text, '"gwet_agreement": null', fixed = TRUE)
  expect_match(json_text, '"gwet_status": "insufficient_variation"', fixed = TRUE)
})

bundled_cal <- function(audit_id = "bundle-a") {
  audit_vitals(
    vitals_samples(),
    human_labels = c("I", "P", "C", "C", "I", "I"),
    label_levels = c("I", "P", "C"), coefficients = "gwet_ac1",
    n_boot = 50, seed = 1, audit_id = audit_id
  )
}

bundled_stability <- function(audit_id = "bundle-a") {
  samples <- vitals_samples()[rep(seq_len(6), each = 3), ]
  samples$epoch <- rep(1:3, times = 6)
  samples$score <- rep(vitals_samples()$score, each = 3)
  stability_vitals(
    samples, content_cols = "input", label_levels = c("I", "P", "C"),
    n_boot = 50, seed = 1, audit_id = audit_id
  )
}

test_that("the vitals adapter uses the score column as the judge column", {
  cal <- bundled_cal()
  expect_identical(cal$judge_col, "score")
  expect_identical(cal$provenance$judge_col, "score")
  expect_false(any(startsWith(names(cal$items), ".yourhonor")))
  expect_identical(cal$items$score, vitals_samples()$score)
  expect_identical(cal$ratings$judge, vitals_samples()$score)
})

test_that("the vitals adapter refuses to overwrite an existing sample column", {
  samples <- vitals_samples()
  samples$target <- c("I", "P", "C", "C", "I", "I")
  expect_error(
    audit_vitals(
      samples, human_labels = samples$target, human_col = "target",
      label_levels = c("I", "P", "C")
    ),
    class = "yourhonor_input_error"
  )
})

test_that("audit_vitals validates samples, labels, and provenance", {
  no_score <- vitals_samples()[c("id", "input")]
  expect_error(
    audit_vitals(no_score, human_labels = rep("C", 6)),
    "missing: score", class = "yourhonor_input_error"
  )
  no_id <- vitals_samples()[c("input", "score")]
  expect_error(
    audit_vitals(no_id, human_labels = rep("C", 6)), "missing: id",
    class = "yourhonor_input_error"
  )
  na_id <- vitals_samples()
  na_id$id[[1]] <- NA
  expect_error(
    audit_vitals(na_id, human_labels = na_id$score),
    class = "yourhonor_input_error"
  )
  expect_error(
    audit_vitals(vitals_samples(), human_labels = c("I", "P")),
    class = "yourhonor_input_error"
  )
  expect_error(
    audit_vitals(
      vitals_samples(), human_labels = vitals_samples()$score,
      provenance = list("unnamed")
    ),
    class = "yourhonor_input_error"
  )
  expect_error(
    audit_vitals(vitals_samples(), human_labels = tibble::tibble(id = 1)),
    class = "yourhonor_input_error"
  )
})

test_that("a task exposing get_samples() is read without another model call", {
  calls <- 0L
  fake_task <- list(get_samples = function() {
    calls <<- calls + 1L
    vitals_samples()
  })
  cal <- audit_vitals(
    fake_task, human_labels = c("I", "P", "C", "C", "I", "I"),
    label_levels = c("I", "P", "C"), coefficients = "gwet_ac1",
    n_boot = 50, seed = 1
  )
  expect_identical(calls, 1L)
  expect_equal(cal$n_items, 6)
  expect_error(
    audit_vitals(list(samples = vitals_samples()), human_labels = rep("C", 6)),
    "get_samples", class = "yourhonor_input_error"
  )
  expect_error(
    audit_vitals("not a task", human_labels = rep("C", 6)),
    "get_samples", class = "yourhonor_input_error"
  )
  expect_error(
    audit_vitals(
      list(get_samples = function() "rows"), human_labels = rep("C", 6)
    ),
    class = "yourhonor_input_error"
  )
  # An R6-style task is an environment, not a list.
  env_task <- new.env()
  env_task$get_samples <- vitals_samples
  expect_equal(
    audit_vitals(
      env_task, human_labels = c("I", "P", "C", "C", "I", "I"),
      label_levels = c("I", "P", "C"), coefficients = "gwet_ac1",
      n_boot = 50, seed = 1
    )$n_items,
    6
  )
})

test_that("vitals stability guards epochs, columns, and content invariance", {
  one_epoch <- vitals_samples()
  one_epoch$epoch <- 1L
  expect_error(
    stability_vitals(one_epoch, content_cols = "input"),
    class = "yourhonor_input_error"
  )
  expect_error(
    stability_vitals(vitals_samples(), content_cols = "input"),
    "missing: epoch", class = "yourhonor_input_error"
  )
  samples <- vitals_samples()[rep(seq_len(6), each = 2), ]
  samples$epoch <- rep(1:2, times = 6)
  expect_error(
    stability_vitals(
      samples[c("id", "epoch", "input")], content_cols = "input"
    ),
    "missing: score", class = "yourhonor_input_error"
  )
  expect_error(
    stability_vitals(samples, content_cols = c("input", "target")),
    "missing: target", class = "yourhonor_input_error"
  )
  expect_error(
    stability_vitals(samples, content_cols = NULL),
    class = "yourhonor_input_error"
  )
  na_epoch <- samples
  na_epoch$epoch[[1]] <- NA
  expect_error(
    stability_vitals(na_epoch, content_cols = "input"),
    class = "yourhonor_input_error"
  )
  changed <- samples
  changed$input[changed$id == "a" & changed$epoch == 2] <- "different result"
  expect_error(
    stability_vitals(changed, content_cols = "input"),
    "changes across epochs for: input", class = "yourhonor_input_error"
  )
})

test_that("vitals stability infers the label universe from a factor score", {
  samples <- vitals_samples()[rep(seq_len(6), each = 2), ]
  samples$epoch <- rep(1:2, times = 6)
  samples$score <- factor(
    rep(c("C", "P", "C", "C", "P", "C"), each = 2), levels = c("I", "P", "C")
  )
  inferred <- stability_vitals(
    samples, content_cols = "input", n_boot = 50, seed = 1
  )
  explicit <- stability_vitals(
    samples, content_cols = "input", label_levels = c("I", "P", "C"),
    n_boot = 50, seed = 1
  )
  expect_identical(inferred$label_levels, c("I", "P", "C"))
  expect_identical(inferred$stability, explicit$stability)
  # A character score column still infers only the observed labels.
  samples$score <- as.character(samples$score)
  observed <- stability_vitals(
    samples, content_cols = "input", n_boot = 50, seed = 1
  )
  expect_setequal(observed$label_levels, c("C", "P"))
})

test_that("JSON audit artifacts round-trip through jsonlite::fromJSON()", {
  cal <- bundled_cal()
  stability <- bundled_stability()
  bias <- probe_bias(
    tibble::tibble(input = paste("answer", 1:30)),
    function(d) ifelse(nchar(d$input) > 40, "C", "P"),
    probes = "verbosity", seed = 1, audit_id = "bundle-a"
  )
  policy <- acceptance_policy(
    min_agreement_lower = 0.9, min_n = 50, min_class_n = 15,
    min_recall = c(I = 0.2, C = 0.3), max_missing_rate = 0,
    max_pairwise_disagreement = 0.1, min_stability_n = 5,
    min_stability_runs = 2, max_stability_missing_rate = 0,
    required_probes = "verbosity", min_probe_n = 20,
    max_probe_flip_rate = 0.1, max_probe_directional_rate = 0.1,
    max_probe_missing_rate = 0, min_precision = c(P = 0.1),
    coefficient = "gwet_ac1"
  )
  path <- withr::local_tempfile(fileext = ".json")
  write_audit(
    cal, path, bias = bias, stability = stability, policy = policy,
    format = "json"
  )
  parsed <- jsonlite::fromJSON(path)
  assessment <- assess_calibration(cal, bias, stability, policy)

  expect_identical(parsed$schema_version, "yourhonor.audit.v1")
  expect_match(
    parsed$created_at, "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$"
  )
  expect_identical(parsed$audit_id, "bundle-a")
  expect_identical(parsed$decision, assessment$decision)
  expect_identical(nrow(parsed$checks), nrow(assessment$checks))
  expect_identical(parsed$checks$check, assessment$checks$check)
  expect_identical(parsed$checks$status, assessment$checks$status)
  # Named class thresholds keep their class names; unset ones stay present.
  expect_identical(parsed$policy$min_recall, list(I = 0.2, C = 0.3))
  expect_identical(parsed$policy$min_precision, list(P = 0.1))
  expect_true("min_human_reliability_lower" %in% names(parsed$policy))
  expect_null(parsed$policy$min_human_reliability_lower)
  expect_identical(parsed$policy$required_probes, "verbosity")
  expect_identical(parsed$bias$audit_id, "bundle-a")
  expect_identical(parsed$bias$results$probe, "verbosity")
  expect_identical(parsed$bias$results$verdict, bias$verdict)
  expect_identical(parsed$stability$audit_id, "bundle-a")
  expect_identical(parsed$stability$n_runs, 3L)
  expect_identical(parsed$stability$provenance$content_cols, "input")
  expect_identical(parsed$calibration$label_levels, c("I", "P", "C"))
  expect_identical(parsed$provenance$judge_col, "score")
  expect_null(parsed$calibration$human_reliability)
})

test_that("JSON audit artifacts keep array shapes, nulls, and full precision", {
  cal <- bundled_cal()
  bias <- probe_bias(
    tibble::tibble(input = paste("answer", 1:30)),
    function(d) ifelse(nchar(d$input) > 40, "C", "P"),
    probes = "verbosity", seed = 1, audit_id = "bundle-a"
  )
  path <- withr::local_tempfile(fileext = ".json")
  write_audit(cal, path, bias = bias, format = "json")
  json_text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  # Length-one vector fields must still be arrays, not scalars.
  expect_match(json_text, '"coefficients": ["gwet_ac1"]', fixed = TRUE)
  expect_match(json_text, '"human_cols": ["human_label"]', fixed = TRUE)
  expect_match(json_text, '"input_cols": []', fixed = TRUE)
  # R NULL is spelled null, never `{}`.
  expect_match(json_text, '"policy": null', fixed = TRUE)
  expect_match(json_text, '"stability": null', fixed = TRUE)
  expect_match(json_text, '"human_reliability": null', fixed = TRUE)
  expect_false(grepl("{}", json_text, fixed = TRUE))
  # A p-value far below 5e-5 survives instead of collapsing to 0.
  parsed <- jsonlite::fromJSON(path)
  expect_lt(bias$p_value, 1e-4)
  expect_equal(parsed$bias$results$p_value, bias$p_value)
  expect_gt(parsed$bias$results$p_value, 0)

  unbundled <- bundled_cal(audit_id = NULL)
  policy <- acceptance_policy(
    min_agreement_lower = 0.9, min_n = 50, min_class_n = 15,
    min_recall = 0.8, max_missing_rate = 0,
    max_pairwise_disagreement = NULL, coefficient = "gwet_ac1"
  )
  path <- withr::local_tempfile(fileext = ".json")
  write_audit(unbundled, path, policy = policy, format = "json")
  json_text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(json_text, '"audit_id": null', fixed = TRUE)
  expect_match(json_text, '"required_probes": []', fixed = TRUE)
  parsed <- jsonlite::fromJSON(path)
  expect_identical(parsed$policy$min_recall, 0.8)
  expect_null(parsed$audit_id)
})

test_that("write_audit() validates path and refuses to overwrite by default", {
  cal <- bundled_cal()
  expect_error(write_audit(cal, 1), class = "yourhonor_input_error")
  expect_error(write_audit(cal, NA_character_), class = "yourhonor_input_error")
  expect_error(
    write_audit(cal, c("a.json", "b.json")), class = "yourhonor_input_error"
  )
  path <- withr::local_tempfile(fileext = ".json")
  write_audit(cal, path, format = "json")
  first <- readLines(path, warn = FALSE)
  expect_error(
    write_audit(cal, path, format = "json"),
    class = "yourhonor_input_error"
  )
  expect_error(
    write_audit(cal, path, format = "md"),
    class = "yourhonor_input_error"
  )
  expect_identical(readLines(path, warn = FALSE), first)
  expect_error(
    write_audit(cal, path, format = "json", overwrite = "yes"),
    class = "yourhonor_input_error"
  )
  expect_identical(
    write_audit(cal, path, format = "md", overwrite = TRUE), path
  )
  expect_match(
    paste(readLines(path, warn = FALSE), collapse = "\n"), "Judge evidence"
  )
})

test_that("write_audit() refuses a directory path", {
  cal <- bundled_cal()
  dir <- withr::local_tempdir()
  expect_error(write_audit(cal, dir), class = "yourhonor_input_error")
  expect_error(
    write_audit(cal, dir, overwrite = TRUE),
    class = "yourhonor_input_error"
  )
})

test_that("any object with a callable get_samples() is a task", {
  setClass("yourhonorFakeTask", representation(n = "numeric"))
  setMethod("$", "yourhonorFakeTask", function(x, name) {
    if (name == "get_samples") vitals_samples else NULL
  })
  cal <- audit_vitals(
    new("yourhonorFakeTask", n = 1),
    human_labels = c("I", "P", "C", "C", "I", "I"),
    label_levels = c("I", "P", "C"), coefficients = "gwet_ac1",
    n_boot = 20, seed = 1
  )
  expect_s3_class(cal, "yourhonor_calibration")
  expect_error(
    audit_vitals(42, human_labels = rep("C", 6)), "get_samples",
    class = "yourhonor_input_error"
  )
  expect_error(
    audit_vitals(NULL, human_labels = rep("C", 6)), "get_samples",
    class = "yourhonor_input_error"
  )
})

test_that("JSON numbers round-trip exactly and provenance keeps its shape", {
  cal <- audit_vitals(
    vitals_samples(),
    human_labels = c("I", "P", "C", "C", "I", "I"),
    label_levels = c("I", "P", "C"), coefficients = "gwet_ac1",
    n_boot = 50, seed = 1, audit_id = "bundle-a",
    provenance = list(temperature = 0.1 + 0.2, content_cols = "input")
  )
  bias <- probe_bias(
    tibble::tibble(input = paste("answer", 1:30)),
    function(d) ifelse(nchar(d$input) > 40, "C", "P"),
    probes = "verbosity", seed = 1, audit_id = "bundle-a"
  )
  path <- withr::local_tempfile(fileext = ".json")
  write_audit(cal, path, bias = bias, format = "json")
  parsed <- jsonlite::fromJSON(path)
  expect_identical(parsed$bias$results$p_value, bias$p_value)
  expect_identical(parsed$provenance$temperature, 0.1 + 0.2)
  expect_identical(parsed$calibration$conf_level, 0.95)
  expect_identical(
    parsed$calibration$agreement$conf_low, cal$agreement$conf_low
  )
  json_text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  # Caller-supplied vector-by-contract keys are arrays too.
  expect_match(json_text, '"content_cols": ["input"]', fixed = TRUE)
  expect_match(json_text, '"id_col": "id"', fixed = TRUE)
  expect_identical(parsed$provenance$created_at, cal$provenance$created_at)
  expect_match(
    parsed$provenance$created_at,
    "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$"
  )

  unbundled <- bundled_cal(audit_id = NULL)
  path <- withr::local_tempfile(fileext = ".json")
  write_audit(unbundled, path, format = "json")
  parsed <- jsonlite::fromJSON(path)
  expect_true("audit_id" %in% names(parsed$provenance))
  expect_null(parsed$provenance$audit_id)
})

test_that("vitals stability drops a blank unused factor level", {
  samples <- vitals_samples()[rep(seq_len(6), each = 2), ]
  samples$epoch <- rep(1:2, times = 6)
  samples$score <- factor(
    rep(vitals_samples()$score, each = 2), levels = c("I", "P", "C", " ")
  )
  st <- stability_vitals(
    samples, content_cols = "input", n_boot = 20, seed = 1
  )
  expect_identical(st$label_levels, c("I", "P", "C"))
  expect_error(
    stability_vitals(
      samples, content_cols = "input", weights = "ordinal", n_boot = 20
    ),
    class = "yourhonor_input_error"
  )
})

# A multi-rater calibration with one item rated once, so every calibration
# table (human reliability, per-rater agreement) and count is written.
multi_rater_bundle <- function() {
  gold <- tibble::tibble(
    human_1 = rep(c("I", "P", "C"), times = 8),
    human_2 = c(NA, rep(c("I", "P", "C"), times = 8)[-1]),
    judge_label = rep(c("I", "P", "C"), times = 8)
  )
  expect_warning(
    cal <- calibrate_judge(
      gold, human_cols = c("human_1", "human_2"),
      label_levels = c("I", "P", "C"), coefficients = "gwet_ac1",
      n_boot = 50, seed = 1, audit_id = "bundle-a"
    ),
    "lacked a usable consensus"
  )
  bias <- probe_bias(
    tibble::tibble(input = paste("answer", 1:30)),
    function(d) rep("C", nrow(d)), probes = "verbosity", seed = 1,
    audit_id = "bundle-a"
  )
  # Integer thresholds, as a caller may type them, must still be written as
  # JSON doubles.
  policy <- acceptance_policy(
    min_agreement_lower = 0.5, min_n = 5, min_class_n = 2,
    min_recall = c(I = 0L, n = 1L), min_precision = 0L,
    max_missing_rate = 0.5,
    max_pairwise_disagreement = 0.5, min_stability_n = 2,
    min_stability_runs = 2, max_stability_missing_rate = 0,
    required_probes = "verbosity", min_probe_n = 5,
    max_probe_flip_rate = 0.5, max_probe_directional_rate = 0.5,
    max_probe_missing_rate = 0, coefficient = "gwet_ac1"
  )
  list(cal = cal, bias = bias, stability = bundled_stability(), policy = policy)
}

write_bundle <- function(bundle) {
  path <- withr::local_tempfile(fileext = ".json", .local_envir = parent.frame())
  write_audit(
    bundle$cal, path, bias = bundle$bias, stability = bundle$stability,
    policy = bundle$policy, format = "json"
  )
  path
}

# The schema's counts are integers; every other number is a double. A
# class-specific threshold is keyed by class name, which says nothing about
# its type (a class may be called "n").
json_count_field <- function(path) {
  if (grepl("^\\.policy\\.min_(recall|precision)\\.", path)) return(FALSE)
  name <- sub("^.*\\.", "", path)
  name == "n" || startsWith(name, "n_") ||
    endsWith(name, "_n_boot_effective") ||
    name %in% c(
      "seed", "support", "predicted", "statistic", "min_n", "min_class_n",
      "min_stability_n", "min_stability_runs", "min_probe_n"
    )
}

json_numbers <- function(x, path = "") {
  if (is.list(x)) {
    keys <- names(x)
    out <- list()
    for (i in seq_along(x)) {
      key <- if (is.null(keys)) "" else keys[[i]]
      # Caller-supplied provenance is serialized verbatim, so its types are
      # the caller's.
      if (identical(key, "provenance")) next
      child <- if (nzchar(key)) paste0(path, ".", key) else path
      out <- c(out, json_numbers(x[[i]], child))
    }
    return(out)
  }
  if (is.numeric(x)) {
    return(list(list(path = path, integer = is.integer(x), value = x)))
  }
  list()
}

test_that("JSON numbers keep one type per field: counts integer, the rest double", {
  path <- write_bundle(multi_rater_bundle())
  parsed <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  numbers <- json_numbers(parsed)
  paths <- vapply(numbers, function(n) n$path, character(1))
  is_int <- vapply(numbers, function(n) n$integer, logical(1))
  expected <- vapply(paths, json_count_field, logical(1))
  expect_identical(unique(paths[is_int != expected]), character(0))
  # The fixture writes whole-valued doubles, which used to come out as JSON
  # integers (`0`, `1`) and change a field's type from run to run.
  whole_doubles <- vapply(numbers, function(n) {
    !n$integer && n$value == round(n$value)
  }, logical(1))
  expect_gt(sum(whole_doubles), 5)
  json_text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(json_text, '"missing_rate": 0.0', fixed = TRUE)
  expect_match(json_text, '"n_runs": 3,', fixed = TRUE)
  expect_match(json_text, '"seed": 1,', fixed = TRUE)
})

test_that("JSON writes the documented calibration and stability scalars", {
  bundle <- multi_rater_bundle()
  path <- write_bundle(bundle)
  parsed <- jsonlite::fromJSON(path)
  cal <- bundle$cal
  stability <- bundle$stability
  expect_identical(cal$n_insufficient_human_ratings, 1L)
  expect_identical(parsed$calibration$n_insufficient_human_ratings, 1L)
  expect_identical(
    names(parsed$stability),
    c(
      "audit_id", "n_items", "n_items_scored", "n_runs", "label_levels",
      "weights", "conf_level", "n_boot", "pairwise_n_boot_effective",
      "stability_n_boot_effective", "n_boot_effective",
      "pairwise_disagreement", "pairwise_disagreement_conf_low",
      "pairwise_disagreement_conf_high", "percent_agreement",
      "any_flip_rate_at_n_runs", "gwet_agreement", "gwet_coefficient",
      "gwet_conf_low", "gwet_conf_high", "gwet_status",
      "gwet_boundary_degenerate", "missing_rate", "pairwise_ci_reliable",
      "stability_ci_reliable", "ci_reliable", "provenance"
    )
  )
  expect_identical(
    parsed$stability$any_flip_rate_at_n_runs, stability$any_flip_rate_at_n_runs
  )
  expect_identical(
    parsed$stability$percent_agreement, stability$percent_agreement
  )
  expect_identical(
    parsed$stability$n_boot_effective, stability$n_boot_effective
  )
  expect_identical(parsed$stability$ci_reliable, stability$ci_reliable)
})
