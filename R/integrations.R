#' Audit grades from a vitals task
#'
#' Extracts samples from a `vitals::Task`, joins blind human labels, and delegates
#' to [calibrate_judge()]. No model is called by this adapter.
#'
#' @param task A `vitals::Task`-like object with a `get_samples()` method, or a
#'   samples data frame for testing/offline use.
#' @param human_labels A label vector aligned with samples, or a data frame keyed
#'   by `id_col` and containing `human_col`.
#' @param id_col Unique sample ID column.
#' @param human_col Human-label column name.
#' @param score_col Vitals score column containing judge grades.
#' @param provenance Optional audit metadata merged with adapter metadata.
#' @param ... Additional arguments passed to [calibrate_judge()].
#'
#' @return A `yourhonor_calibration` object.
#' @section Repeated epochs:
#' A vitals task with multiple epochs contains duplicated item IDs. This adapter
#' deliberately rejects those rows for calibration because treating reruns as
#' independent gold items would make confidence intervals too narrow. Filter to
#' one epoch before calling `audit_vitals()`. Use [stability_vitals()] for epochs
#' only when every judge-visible content field is invariant within item.
#' @export
audit_vitals <- function(task,
                         human_labels,
                         id_col = "id",
                         human_col = "human_label",
                         score_col = "score",
                         provenance = list(),
                         ...) {
  id_col <- check_column_name(id_col, "id_col")
  human_col <- check_column_name(human_col, "human_col")
  score_col <- check_column_name(score_col, "score_col")
  if (anyDuplicated(c(id_col, human_col, score_col))) {
    cli::cli_abort("{.arg id_col}, {.arg human_col}, and {.arg score_col} must be distinct.")
  }
  samples <- extract_vitals_samples(task)
  missing_cols <- setdiff(c(id_col, score_col), names(samples))
  if (length(missing_cols) > 0L) {
    cli::cli_abort("Vitals samples are missing: {.field {missing_cols}}.")
  }
  if (anyNA(samples[[id_col]])) {
    cli::cli_abort("Vitals sample IDs must be non-missing.")
  }
  if (anyDuplicated(samples[[id_col]])) {
    cli::cli_abort(paste0(
      "Vitals sample IDs must be unique for calibration. ",
      "Multi-epoch rows are repeated measurements, not independent gold items; ",
      "filter to one epoch first."
    ))
  }

  if (is.data.frame(human_labels)) {
    human_labels <- tibble::as_tibble(human_labels)
    missing_human <- setdiff(c(id_col, human_col), names(human_labels))
    if (length(missing_human) > 0L) {
      cli::cli_abort("Human labels are missing: {.field {missing_human}}.")
    }
    if (anyNA(human_labels[[id_col]]) || anyDuplicated(human_labels[[id_col]])) {
      cli::cli_abort("Human-label IDs must be unique and non-missing.")
    }
    if (!setequal(samples[[id_col]], human_labels[[id_col]]) ||
        nrow(samples) != nrow(human_labels)) {
      cli::cli_abort("Human-label IDs must exactly match the vitals sample IDs.")
    }
    index <- match(samples[[id_col]], human_labels[[id_col]])
    labels <- human_labels[[human_col]][index]
  } else {
    if (length(human_labels) != nrow(samples)) {
      cli::cli_abort("A human-label vector must have one value per vitals sample.")
    }
    labels <- human_labels
  }

  judge_labels <- samples[[score_col]]
  judge_col <- ".yourhonor_judge_label"
  check_labels_nonblank(as.character(labels), "human_labels")
  check_labels_nonblank(as.character(judge_labels), "score_col")
  samples[[human_col]] <- labels
  samples[[judge_col]] <- judge_labels
  if (!is.list(provenance) || (length(provenance) > 0L && is.null(names(provenance)))) {
    cli::cli_abort("{.arg provenance} must be a named list (an empty list is allowed).")
  }
  adapter_provenance <- utils::modifyList(
    provenance,
    list(source = "vitals", score_col = score_col)
  )
  calibrate_judge(
    samples,
    judge = NULL,
    id_col = id_col,
    human_cols = human_col,
    judge_col = judge_col,
    provenance = adapter_provenance,
    ...
  )
}

#' Measure stability from precomputed vitals epochs
#'
#' Converts long-form vitals samples into an item-by-epoch rating matrix and
#' computes the same stability evidence as [test_retest()] without calling a
#' model. Repeated epochs are treated as repeated measurements of the same item,
#' never as independent calibration rows.
#'
#' @param task A `vitals::Task`-like object with a `get_samples()` method, or a
#'   samples data frame.
#' @param id_col Item identifier shared across epochs.
#' @param epoch_col Epoch/run identifier.
#' @param score_col Precomputed judge-score column.
#' @param content_cols Every column visible to the judge (typically `input`,
#'   `target`, and `result`). Values must be invariant within item ID; otherwise
#'   epochs measure changing evaluation outcomes, not judge test-retest
#'   reliability. Identifier, epoch, and score columns are not content columns.
#' @param label_levels Optional complete label universe.
#' @param weights Agreement weights passed to Gwet's coefficient.
#' @param conf_level Bootstrap confidence level.
#' @param n_boot Number of item-bootstrap resamples.
#' @param seed Optional bootstrap seed.
#' @param audit_id Optional evidence-bundle identifier shared with calibration.
#'
#' @return A `yourhonor_stability` object with `source = "vitals"` provenance.
#' @export
stability_vitals <- function(task,
                             id_col = "id",
                             epoch_col = "epoch",
                             score_col = "score",
                             content_cols = NULL,
                             label_levels = NULL,
                             weights = c("unweighted", "linear", "quadratic", "ordinal"),
                             conf_level = 0.95,
                             n_boot = 1000,
                             seed = NULL,
                             audit_id = NULL) {
  id_col <- check_column_name(id_col, "id_col")
  epoch_col <- check_column_name(epoch_col, "epoch_col")
  score_col <- check_column_name(score_col, "score_col")
  if (is.null(content_cols) || length(content_cols) == 0L) {
    cli::cli_abort(
      "{.arg content_cols} must list every judge-visible field for invariance checks."
    )
  }
  content_cols <- check_column_names(content_cols, "content_cols")
  if (anyDuplicated(c(id_col, epoch_col, score_col))) {
    cli::cli_abort(
      "{.arg id_col}, {.arg epoch_col}, and {.arg score_col} must be distinct."
    )
  }
  reserved_cols <- intersect(content_cols, c(id_col, epoch_col, score_col))
  if (length(reserved_cols) > 0L) {
    cli::cli_abort(paste0(
      "{.arg content_cols} must not include identifier, epoch, or score columns: ",
      "{.field {reserved_cols}}."
    ))
  }
  weights <- rlang::arg_match(weights)
  if (weights != "unweighted" && is.null(label_levels)) {
    cli::cli_abort(
      "{.arg label_levels} is required for weighted stability; ordinal order cannot be inferred."
    )
  }
  conf_level <- check_probability(conf_level, "conf_level", open = TRUE)
  n_boot <- check_count(n_boot, "n_boot")
  audit_id <- check_audit_id(audit_id)
  samples <- extract_vitals_samples(task)
  missing_cols <- setdiff(
    c(id_col, epoch_col, score_col, content_cols), names(samples)
  )
  if (length(missing_cols) > 0L) {
    cli::cli_abort("Vitals samples are missing: {.field {missing_cols}}.")
  }
  key <- samples[c(id_col, epoch_col)]
  if (anyNA(key)) {
    cli::cli_abort("Vitals item and epoch identifiers must be non-missing.")
  }
  if (anyDuplicated(as.data.frame(key))) {
    cli::cli_abort("Every vitals item-epoch key must appear exactly once.")
  }
  changed <- vapply(content_cols, function(column) {
    values <- split(samples[[column]], samples[[id_col]])
    any(vapply(values, function(value) length(unique(value)) > 1L, logical(1)))
  }, logical(1))
  if (any(changed)) {
    cli::cli_abort(paste0(
      "Judge-visible content changes across epochs for: {.field {content_cols[changed]}}. ",
      "Those rows measure end-to-end evaluation changes, not judge test-retest stability."
    ))
  }
  item_ids <- unique(samples[[id_col]])
  epochs <- unique(samples[[epoch_col]])
  if (length(item_ids) < 1L || length(epochs) < 2L) {
    cli::cli_abort("Vitals stability needs at least one item and two epochs.")
  }
  runs <- matrix(
    NA_character_, nrow = length(item_ids), ncol = length(epochs),
    dimnames = list(as.character(item_ids), as.character(epochs))
  )
  locations <- cbind(
    match(samples[[id_col]], item_ids),
    match(samples[[epoch_col]], epochs)
  )
  runs[locations] <- as.character(samples[[score_col]])
  if (!is.null(seed)) withr::local_seed(seed)
  out <- summarize_stability_runs(
    runs, label_levels = label_levels, weights = weights,
    conf_level = conf_level, n_boot = n_boot, audit_id = audit_id,
    arg = "score_col"
  )
  out$provenance <- list(
    source = "vitals", id_col = id_col, epoch_col = epoch_col,
    score_col = score_col, content_cols = content_cols, seed = seed
  )
  out
}

extract_vitals_samples <- function(task) {
  if (is.data.frame(task)) {
    tibble::as_tibble(task)
  } else if (!is.null(task$get_samples) && is.function(task$get_samples)) {
    tibble::as_tibble(task$get_samples())
  } else {
    cli::cli_abort(
      "{.arg task} must be a samples data frame or expose a callable {.fn get_samples}."
    )
  }
}

#' Write a portable judge audit artifact
#'
#' Writes either the Markdown evidence report or a versioned JSON summary. The
#' JSON artifact does not automatically serialize `x$items`; it contains
#' aggregate evidence, policy checks, and provenance.
#'
#' @param x A `yourhonor_calibration` object.
#' @param path Output path.
#' @param bias Optional bias evidence.
#' @param stability Optional stability evidence.
#' @param policy Optional acceptance policy.
#' @param format `"json"` or `"md"`.
#' @return `path`, invisibly.
#' @section Privacy:
#' Calibration item rows are omitted from JSON. Caller-supplied `provenance` is
#' serialized verbatim, however, so do not place secrets or raw item content in
#' provenance fields.
#' @export
write_audit <- function(x,
                        path,
                        bias = NULL,
                        stability = NULL,
                        policy = NULL,
                        format = c("json", "md")) {
  if (!inherits(x, "yourhonor_calibration")) {
    cli::cli_abort("{.arg x} must be a {.cls yourhonor_calibration} object.")
  }
  validate_report_evidence(x, bias, stability)
  format <- rlang::arg_match(format)
  if (length(path) != 1L || is.na(path) || !nzchar(path)) {
    cli::cli_abort("{.arg path} must be one non-empty path.")
  }
  if (format == "md") {
    md <- report(x, bias = bias, stability = stability, policy = policy, output = "md")
    writeLines(as.character(md), path, useBytes = TRUE)
    return(invisible(path))
  }
  if (!supplied_evidence_is_bound(x, bias, stability)) {
    cli::cli_abort(paste0(
      "JSON audit artifacts cannot include UNBOUND secondary evidence. ",
      "Use one {.arg audit_id} across calibration, bias, and stability."
    ))
  }

  rlang::check_installed("jsonlite", reason = "to write a JSON audit artifact")
  assessment <- assess_calibration(x, bias, stability, policy)
  payload <- list(
    schema_version = "yourhonor.audit.v1",
    created_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    decision = assessment$decision,
    audit_id = x$audit_id,
    policy = if (is.null(policy)) NULL else unclass(policy),
    checks = as.data.frame(assessment$checks),
    calibration = list(
      n_input = x$n_input,
      n_items = x$n_items,
      n_excluded = x$n_excluded,
      n_consensus_ties = x$n_consensus_ties,
      label_levels = x$label_levels,
      scale = x$scale,
      weights = x$weights,
      conf_level = x$conf_level,
      n_boot = x$n_boot,
      seed = x$seed,
      coefficients = x$coefficients,
      agreement = as.data.frame(x$agreement),
      confusion_metrics = as.data.frame(x$confusion_metrics),
      human_reliability = if (is.null(x$human_reliability)) NULL else
        as.data.frame(x$human_reliability)
    ),
    bias = if (is.null(bias)) NULL else list(
      audit_id = evidence_audit_id(bias),
      results = as.data.frame(bias)
    ),
    stability = if (is.null(stability)) NULL else list(
      audit_id = stability$audit_id,
      n_items = stability$n_items,
      n_items_scored = stability$n_items_scored,
      n_runs = stability$n_runs,
      label_levels = stability$label_levels,
      weights = stability$weights,
      conf_level = stability$conf_level,
      n_boot = stability$n_boot,
      pairwise_n_boot_effective = stability$pairwise_n_boot_effective,
      stability_n_boot_effective = stability$stability_n_boot_effective,
      pairwise_disagreement = stability$pairwise_disagreement,
      pairwise_disagreement_conf_low = stability$pairwise_disagreement_conf_low,
      pairwise_disagreement_conf_high = stability$pairwise_disagreement_conf_high,
      gwet_agreement = stability$stability,
      gwet_coefficient = stability$stability_coefficient,
      gwet_conf_low = stability$stability_conf_low,
      gwet_conf_high = stability$stability_conf_high,
      gwet_status = stability$stability_status,
      gwet_boundary_degenerate = stability$stability_boundary_degenerate,
      missing_rate = stability$missing_rate,
      pairwise_ci_reliable = stability$pairwise_ci_reliable,
      stability_ci_reliable = stability$stability_ci_reliable,
      provenance = stability$provenance
    ),
    provenance = x$provenance
  )
  jsonlite::write_json(payload, path, auto_unbox = TRUE, pretty = TRUE, na = "null")
  invisible(path)
}
