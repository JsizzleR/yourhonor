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
    "exactly match"
  )
  labels$id[[2]] <- labels$id[[1]]
  expect_error(
    audit_vitals(vitals_samples(), labels, label_levels = c("I", "P", "C")),
    "unique"
  )
  expect_error(
    audit_vitals(
      vitals_samples(), human_labels = vitals_samples()$score,
      id_col = c("id", "input"),
      label_levels = c("I", "P", "C")
    ),
    "one non-empty"
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
    "Multi-epoch"
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
    "distinct"
  )
  expect_error(
    stability_vitals(samples, content_cols = "id"),
    "must not include identifier"
  )
  expect_error(
    stability_vitals(samples, content_cols = "epoch"),
    "must not include identifier"
  )
  expect_error(
    stability_vitals(samples, content_cols = "score"),
    "must not include identifier"
  )
  duplicated <- vctrs::vec_rbind(samples, samples[1, ])
  expect_error(stability_vitals(duplicated, content_cols = "input"), "exactly once")
  expect_error(stability_vitals(vitals_samples(), content_cols = "input"), "epoch")
  changed <- samples
  changed$input[changed$id == "a" & changed$epoch == 2] <- "different result"
  expect_error(
    stability_vitals(changed, content_cols = "input"),
    "end-to-end evaluation changes"
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
    "distinct"
  )
})

test_that("audit artifacts omit raw item text", {
  cal <- audit_vitals(
    vitals_samples(),
    human_labels = c("I", "P", "C", "C", "I", "I"),
    label_levels = c("I", "P", "C"), coefficients = "gwet_ac1",
    n_boot = 50, seed = 1
  )
  json <- tempfile(fileext = ".json")
  md <- tempfile(fileext = ".md")
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
  expect_error(
    write_audit(cal, tempfile(fileext = ".json"), bias = bias, format = "json"),
    "UNBOUND"
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
  path <- tempfile(fileext = ".json")
  write_audit(cal, path, stability = stability, format = "json")
  json_text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(json_text, '"gwet_agreement": null', fixed = TRUE)
  expect_match(json_text, '"gwet_status": "insufficient_variation"', fixed = TRUE)
})
