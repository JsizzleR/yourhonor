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
#' @examples
#' # A samples data frame stands in for a vitals task here; with vitals
#' # installed, pass the task itself and its `get_samples()` is read.
#' samples <- tibble::tibble(
#'   id = paste0("q", 1:8),
#'   input = paste("question", 1:8),
#'   score = c("C", "C", "P", "I", "C", "P", "I", "C")
#' )
#' # Human labels keyed by sample ID, in any order.
#' human <- tibble::tibble(
#'   id = paste0("q", 8:1),
#'   human_label = c("C", "I", "C", "C", "I", "P", "C", "C")
#' )
#' cal <- audit_vitals(samples, human, label_levels = c("I", "P", "C"),
#'                     n_boot = 200, seed = 1)
#' cal
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
    yh_abort("{.arg id_col}, {.arg human_col}, and {.arg score_col} must be distinct.")
  }
  check_provenance(provenance)
  samples <- extract_vitals_samples(task)
  missing_cols <- setdiff(c(id_col, score_col), names(samples))
  if (length(missing_cols) > 0L) {
    yh_abort("Vitals samples are missing: {.field {missing_cols}}.")
  }
  # Human labels are joined onto the samples under `human_col`, so an existing
  # column of that name (for example a vitals `target`) would be replaced
  # silently and the audit would compare the judge against the wrong labels.
  if (human_col %in% names(samples)) {
    yh_abort(paste0(
      "Vitals samples already contain a {.field {human_col}} column; ",
      "choose a {.arg human_col} that is not present in the samples."
    ))
  }
  if (anyNA(samples[[id_col]])) {
    yh_abort("Vitals sample IDs must be non-missing.")
  }
  if (anyDuplicated(samples[[id_col]])) {
    yh_abort(paste0(
      "Vitals sample IDs must be unique for calibration. ",
      "Multi-epoch rows are repeated measurements, not independent gold items; ",
      "filter to one epoch first."
    ))
  }

  if (is.data.frame(human_labels)) {
    human_labels <- tibble::as_tibble(human_labels)
    missing_human <- setdiff(c(id_col, human_col), names(human_labels))
    if (length(missing_human) > 0L) {
      yh_abort("Human labels are missing: {.field {missing_human}}.")
    }
    if (anyNA(human_labels[[id_col]]) || anyDuplicated(human_labels[[id_col]])) {
      yh_abort("Human-label IDs must be unique and non-missing.")
    }
    if (!setequal(samples[[id_col]], human_labels[[id_col]]) ||
        nrow(samples) != nrow(human_labels)) {
      yh_abort("Human-label IDs must exactly match the vitals sample IDs.")
    }
    index <- match(samples[[id_col]], human_labels[[id_col]])
    labels <- human_labels[[human_col]][index]
  } else {
    if (length(human_labels) != nrow(samples)) {
      yh_abort("A human-label vector must have one value per vitals sample.")
    }
    labels <- human_labels
  }

  # The vitals score column *is* the judge-label column. Reusing its name keeps
  # `cal$judge_col`, `cal$items`, and the JSON provenance free of internal
  # sentinel names; `score_col != human_col` is already guaranteed above.
  check_labels_nonblank(as.character(labels), "human_labels")
  check_labels_nonblank(as.character(samples[[score_col]]), "score_col")
  samples[[human_col]] <- labels
  adapter_provenance <- utils::modifyList(
    provenance,
    list(source = "vitals", score_col = score_col)
  )
  calibrate_judge(
    samples,
    judge = NULL,
    id_col = id_col,
    human_cols = human_col,
    judge_col = score_col,
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
#' @param label_levels Optional complete label universe. For unweighted runs
#'   left `NULL` with a factor `score_col`, the factor's non-blank levels are
#'   used as the label universe; a level no epoch used changes no unweighted
#'   coefficient (see [agreement()]). A numeric `NaN` score is a missing
#'   judgment, not a label. Weighted runs always require it: a factor's level order is
#'   not taken as evidence of the ordinal order.
#' @param weights Agreement weights passed to Gwet's coefficient.
#' @param conf_level Bootstrap confidence level.
#' @param n_boot Number of item-bootstrap resamples.
#' @param seed Optional bootstrap seed.
#' @param audit_id Optional evidence-bundle identifier shared with calibration.
#'
#' @return A `yourhonor_stability` object with `source = "vitals"` provenance.
#' @export
#' @examples
#' # Three epochs of six items, in long form as vitals stores them; the
#' # judge-visible `input` is the same in every epoch.
#' samples <- tibble::tibble(
#'   id = rep(paste0("q", 1:6), each = 3),
#'   epoch = rep(1:3, times = 6),
#'   input = rep(paste("question", 1:6), each = 3),
#'   score = c("C", "C", "C", "P", "P", "C", "I", "I", "I",
#'             "C", "C", "C", "P", "P", "P", "I", "P", "I")
#' )
#' stability_vitals(samples, content_cols = "input",
#'                  label_levels = c("I", "P", "C"), n_boot = 200, seed = 1)
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
    yh_abort(
      "{.arg content_cols} must list every judge-visible field for invariance checks."
    )
  }
  content_cols <- check_column_names(content_cols, "content_cols")
  if (anyDuplicated(c(id_col, epoch_col, score_col))) {
    yh_abort(
      "{.arg id_col}, {.arg epoch_col}, and {.arg score_col} must be distinct."
    )
  }
  reserved_cols <- intersect(content_cols, c(id_col, epoch_col, score_col))
  if (length(reserved_cols) > 0L) {
    yh_abort(paste0(
      "{.arg content_cols} must not include identifier, epoch, or score columns: ",
      "{.field {reserved_cols}}."
    ))
  }
  weights <- rlang::arg_match(weights)
  if (weights != "unweighted" && is.null(label_levels)) {
    yh_abort(
      "{.arg label_levels} is required for weighted stability; ordinal order cannot be inferred."
    )
  }
  conf_level <- check_probability(conf_level, "conf_level", open = TRUE)
  n_boot <- check_count(n_boot, "n_boot")
  seed <- check_seed(seed)
  audit_id <- check_audit_id(audit_id)
  samples <- extract_vitals_samples(task)
  missing_cols <- setdiff(
    c(id_col, epoch_col, score_col, content_cols), names(samples)
  )
  if (length(missing_cols) > 0L) {
    yh_abort("Vitals samples are missing: {.field {missing_cols}}.")
  }
  # as.character() would turn a list cell into a deparsed label ("NA",
  # "NULL") rather than a missing judgment, so a list of one score (or none)
  # per sample is unpacked first; a matrix column is several scores per row.
  # The same shape contract grade_keyed() applies to a judge's labels.
  scores <- samples[[score_col]]
  if (is.list(scores) && is.null(dim(scores))) {
    scores <- unpack_list_scores(scores, score_col)
  }
  if (!is.atomic(scores) || !is.null(dim(scores))) {
    yh_abort(paste0(
      "{.arg score_col} {.field {score_col}} must be an atomic vector (one ",
      "score per sample, with no {.code dim}), not {.cls {class(scores)}}."
    ))
  }
  # The rating matrix below is character, so factor levels would be lost
  # before `resolve_label_levels()` could infer the label universe from them.
  if (is.null(label_levels) && is.factor(samples[[score_col]])) {
    label_levels <- inferred_factor_levels(samples[[score_col]])
  }
  key <- samples[c(id_col, epoch_col)]
  if (anyNA(key)) {
    yh_abort("Vitals item and epoch identifiers must be non-missing.")
  }
  if (anyDuplicated(as.data.frame(key))) {
    yh_abort("Every vitals item-epoch key must appear exactly once.")
  }
  changed <- vapply(content_cols, function(column) {
    values <- split(samples[[column]], samples[[id_col]])
    any(vapply(values, function(value) length(unique(value)) > 1L, logical(1)))
  }, logical(1))
  if (any(changed)) {
    yh_abort(paste0(
      "Judge-visible content changes across epochs for: {.field {content_cols[changed]}}. ",
      "Those rows measure end-to-end evaluation changes, not judge test-retest stability."
    ))
  }
  item_ids <- unique(samples[[id_col]])
  epochs <- unique(samples[[epoch_col]])
  if (length(item_ids) < 1L || length(epochs) < 2L) {
    yh_abort("Vitals stability needs at least one item and two epochs.")
  }
  runs <- matrix(
    NA_character_, nrow = length(item_ids), ncol = length(epochs),
    dimnames = list(as.character(item_ids), as.character(epochs))
  )
  locations <- cbind(
    match(samples[[id_col]], item_ids),
    match(samples[[epoch_col]], epochs)
  )
  runs[locations] <- as_labels(scores)
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

# One character score per list cell; an empty cell, NA or NaN is missing.
unpack_list_scores <- function(scores, score_col) {
  scalar <- vapply(scores, function(value) {
    is.null(value) ||
      (is.atomic(value) && is.null(dim(value)) && length(value) <= 1L)
  }, logical(1))
  if (!all(scalar)) {
    yh_abort(paste0(
      "Each cell of list column {.field {score_col}} must hold one score or ",
      "nothing."
    ))
  }
  vapply(scores, function(value) {
    if (length(value) == 0L) NA_character_ else as_labels(value)
  }, character(1), USE.NAMES = FALSE)
}

extract_vitals_samples <- function(task) {
  if (is.data.frame(task)) {
    return(tibble::as_tibble(task))
  }
  # Any object whose `$get_samples` is callable is accepted (R6 tasks are
  # environments; an S4 task may define its own `$` method). `$` is an R
  # error on atomic vectors, so a failed access is reported as invalid input
  # rather than surfacing as an internal error.
  get_samples <- tryCatch(task$get_samples, error = function(e) NULL)
  if (!is.function(get_samples)) {
    yh_abort(paste0(
      "{.arg task} must be a samples data frame or expose a callable ",
      "{.fn get_samples}."
    ))
  }
  samples <- get_samples()
  if (!is.data.frame(samples)) {
    yh_abort("{.fn get_samples} must return a samples data frame.")
  }
  tibble::as_tibble(samples)
}

#' Write a portable judge audit artifact
#'
#' Writes either the Markdown evidence report or a versioned JSON summary. The
#' JSON artifact does not automatically serialize `x$items`; it contains
#' aggregate evidence, policy checks, and provenance.
#'
#' @param x A `yourhonor_calibration` object.
#' @param path Output file path. An existing file is never replaced unless
#'   `overwrite = TRUE`, so a re-run cannot silently clobber an earlier audit.
#' @param bias Optional bias evidence.
#' @param stability Optional stability evidence.
#' @param policy Optional acceptance policy.
#' @param format `"json"` or `"md"`.
#' @param overwrite Replace an existing file at `path`? Defaults to `FALSE`.
#' @return `path`, invisibly.
#' @section Privacy:
#' Calibration item rows are omitted from JSON. Caller-supplied `provenance` is
#' serialized verbatim, however, so do not place secrets or raw item content in
#' provenance fields.
#' @section JSON schema (`yourhonor.audit.v1`):
#' The JSON artifact is written at full numeric precision: doubles carry 17
#' significant digits, so every value round-trips exactly (a reader may see
#' `0.94999999999999996` for `0.95`). That is the precision of the stored
#' doubles, not of the estimates: irrCAC rounds each agreement and stability
#' coefficient to five decimals, so every `estimate` (and stability's
#' `gwet_agreement`) and every bootstrap draw sits on that grid, and the
#' bootstrap bounds interpolated between draws are no more precise.
#' `conf_low_exact` is computed from irrCAC's unrounded observed and chance
#' agreement and is capped at the rounded `estimate` (and at the bound of the
#' perfect samples it can be corrected into; see [agreement()]). Every absent
#' value is `null`: an `NA` and an R `NULL` are indistinguishable to a reader,
#' but neither ever appears as `{}` or is dropped. Fields that are vectors by
#' contract are always JSON arrays, even when they hold one value. Each
#' numeric field outside caller-supplied `provenance` (whose values are
#' written as the caller typed them) keeps one JSON number type from run to
#' run: `seed` and the
#' counts (`n`, every `n_*` and `*_n_boot_effective` field, `support`,
#' `predicted`, `statistic`, and the policy's `min_n`, `min_class_n`,
#' `min_stability_n`, `min_stability_runs` and `min_probe_n`) are integers,
#' and every other number, including each check's `value` and `threshold`,
#' is written with a decimal point or an exponent (`0.0`, `1.0`), even when
#' it is whole. Top-level members:
#' * `schema_version`: `"yourhonor.audit.v1"`.
#' * `created_at`: UTC timestamp in RFC 3339 form, e.g.
#'   `"2026-01-31T12:00:00Z"`.
#' * `decision`: the overall assessment (`PASS`, `FAIL`, `INDETERMINATE`,
#'   or `NOT_ASSESSED`).
#' * `audit_id`: string or `null`.
#' * `policy`: `null`, or an object with one member per
#'   [acceptance_policy()] argument. A `null` `coefficient` means the
#'   calibration's Gwet coefficient, which the `agreement_<coefficient>`
#'   check names. `required_probes` is an array;
#'   `min_recall` and `min_precision` are a number when global or an object
#'   keyed by class name when class-specific; unset thresholds are `null`.
#' * `checks`: array of objects, one per policy check, with `check`, `status`,
#'   `value` (the number compared with `threshold`, `null` when
#'   unavailable: the observed value, or for an interval check the bound
#'   assessed, which for a bias probe's flip-rate and directional checks
#'   counts every unscored item against the probe over `n_input`: as a flip
#'   for the flip rate, towards the worse direction for the directional
#'   rate, and for the stability disagreement check is
#'   `stability.pairwise_disagreement_worst_case_high`, which counts every
#'   pair involving a missing judgment as a disagreement over all `n_items`,
#'   so it can exceed `stability.pairwise_disagreement_conf_high`; only when
#'   that worst-case bound is unavailable (not a number from 0 to 1, or
#'   below the observed upper unless the evidence shows nothing missing)
#'   does a
#'   stability `FAIL` report the observed upper), `op` (the
#'   comparison the check encodes: `">="`, `"<="`, `"lower >="`,
#'   `"upper <="`, or `"="`), a numeric `threshold`, and `reason`. Without a
#'   policy the single placeholder check has `null` `op` and `threshold`.
#' * `calibration`: `n_input`, `n_items`, `n_excluded`, `n_consensus_ties`,
#'   `n_insufficient_human_ratings`, `n_judge_abstentions`,
#'   `judge_abstention_rate`,
#'   `judge_abstention_conf_low`, `judge_abstention_conf_high`, `scale`,
#'   `weights`, `conf_level`, `n_boot`, `seed` (scalars); `label_levels` and
#'   `coefficients` (arrays); `agreement`, `confusion`, `confusion_metrics`,
#'   `human_reliability`, and `per_rater_agreement` (arrays of row objects
#'   mirroring the tibble columns; `human_reliability` and
#'   `per_rater_agreement` are `null` for a single rater). `confusion` holds
#'   one row per reference-by-judge cell, empty cells included, with the
#'   reference label, the judge label, the count `n`, and its share `prop`,
#'   so a reader can rebuild the matrix in `label_levels` order.
#' * `bias`: `null`, or `audit_id` plus `results`, an array of row objects
#'   with the columns of the bias tibble (see [probe_bias()]), among them
#'   `n_unscored_tied`, an additive column of `yourhonor.audit.v1`.
#' * `stability`: `null`, or the summary fields of a `yourhonor_stability`
#'   object (see [test_retest()]): `audit_id` (string or `null`), `n_items`,
#'   `n_items_scored`, `n_runs`, `label_levels` (array), `weights`,
#'   `conf_level`, `n_boot`, `pairwise_n_boot_effective`,
#'   `stability_n_boot_effective`, `n_boot_effective` (the smaller of the
#'   two), `pairwise_disagreement` with its observed
#'   `pairwise_disagreement_conf_low` and `pairwise_disagreement_conf_high`,
#'   `percent_agreement` (`1 - pairwise_disagreement`, a proportion despite
#'   its name), `any_flip_rate_at_n_runs`, the bounds for missing judgments
#'   (the policy assesses `pairwise_disagreement_worst_case_high` and
#'   `pairwise_disagreement_best_case_low`; `n_items_complete`, the items
#'   with every run observed; `pairwise_disagreement_worst_case` and its
#'   upper bound `pairwise_disagreement_worst_case_high`; `any_flip_rate_worst_case`;
#'   `pairwise_disagreement_best_case` and its lower bound
#'   `pairwise_disagreement_best_case_low`; see [test_retest()]; additive
#'   members of `yourhonor.audit.v1`), the chance-corrected agreement as
#'   `gwet_agreement`, `gwet_coefficient`, `gwet_conf_low`, `gwet_conf_high`,
#'   `gwet_status` and `gwet_boundary_degenerate` (the object's
#'   `stability`, `stability_coefficient`, `stability_conf_low`,
#'   `stability_conf_high`, `stability_status` and
#'   `stability_boundary_degenerate`), `missing_rate`,
#'   `pairwise_ci_reliable`, `stability_ci_reliable`, `ci_reliable` (both
#'   intervals reliable), and `provenance`.
#' * `provenance`: the calibration provenance. The keys `input_cols`,
#'   `human_cols`, `label_levels`, and `content_cols` are always arrays, even
#'   when they were supplied by the caller and hold one value; `created_at`
#'   is RFC 3339 UTC like the top-level field, and adapter fields left `NULL`
#'   (`id_col`, `audit_id`) are present as `null`.
#' @export
#' @examples
#' cal <- calibrate_judge(support_gold, label_levels = c("I", "P", "C"),
#'                        n_boot = 200, seed = 1)
#' path <- tempfile(fileext = ".json")
#' write_audit(cal, path)
#' audit <- jsonlite::read_json(path)
#' audit$schema_version
#' audit$decision
#'
#' md <- tempfile(fileext = ".md")
#' write_audit(cal, md, format = "md")
#' unlink(c(path, md))
write_audit <- function(x,
                        path,
                        bias = NULL,
                        stability = NULL,
                        policy = NULL,
                        format = c("json", "md"),
                        overwrite = FALSE) {
  if (!inherits(x, "yourhonor_calibration")) {
    yh_abort("{.arg x} must be a {.cls yourhonor_calibration} object.")
  }
  validate_report_evidence(x, bias, stability)
  format <- rlang::arg_match(format)
  path <- check_audit_path(path, overwrite)
  if (format == "md") {
    md <- report(x, bias = bias, stability = stability, policy = policy)
    writeLines(format(md, output = "md"), path, useBytes = TRUE)
    return(invisible(path))
  }
  if (!supplied_evidence_is_bound(x, bias, stability)) {
    yh_abort(paste0(
      "JSON audit artifacts cannot include UNBOUND secondary evidence. ",
      "Use one {.arg audit_id} across calibration, bias, and stability."
    ), class = binding_error)
  }

  assessment <- assess_calibration(x, bias, stability, policy)
  payload <- list(
    schema_version = "yourhonor.audit.v1",
    created_at = utc_timestamp(),
    decision = assessment$decision,
    audit_id = audit_id(x),
    policy = json_policy(policy),
    checks = as.data.frame(assessment$checks),
    calibration = json_calibration(x),
    bias = json_bias(bias),
    stability = json_stability(stability),
    provenance = json_provenance(x$provenance)
  )
  # The JSON file is the only precise artifact (the Markdown report rounds to
  # two decimals). `digits = I(17)` writes 17 significant digits, the count
  # that makes every double round-trip exactly (`digits = NA` would round to
  # 15), and an R NULL is spelled `null` like an NA rather than jsonlite's
  # default `{}`. `always_decimal = TRUE` writes a whole-valued double as
  # `1.0`, so a rate or bound that happens to be 0 or 1 stays a JSON double
  # instead of turning into an integer on that run; integer counts are
  # unaffected.
  jsonlite::write_json(
    payload, path,
    auto_unbox = TRUE, pretty = TRUE, digits = I(17), na = "null",
    null = "null", always_decimal = TRUE
  )
  invisible(path)
}

# One timestamp format for every artifact field: RFC 3339 in UTC.
utc_timestamp <- function(time = Sys.time()) {
  format(time, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

check_audit_path <- function(path, overwrite) {
  if (!is.character(path) || length(path) != 1L || is.na(path) ||
      !nzchar(path)) {
    yh_abort("{.arg path} must be one non-empty file path.")
  }
  if (!rlang::is_bool(overwrite)) {
    yh_abort("{.arg overwrite} must be TRUE or FALSE.")
  }
  # A directory would pass the existence test below and, with
  # `overwrite = TRUE`, reach the writer as a target it cannot replace.
  if (dir.exists(path)) {
    yh_abort("{.path {path}} is a directory, not a file path.")
  }
  if (file.exists(path) && !overwrite) {
    yh_abort(paste0(
      "{.path {path}} already exists; set {.code overwrite = TRUE} ",
      "to replace it."
    ))
  }
  path
}

# `auto_unbox = TRUE` turns every length-one vector into a JSON scalar, so a
# reader could not tell "one coefficient" from "a coefficient". Fields that
# are vectors by contract are wrapped so they always serialize as arrays.
json_array <- function(x) {
  if (is.null(x)) return(NULL)
  I(x)
}

# A class-specific threshold keeps its names only as a list; `unclass()` alone
# yields a bare numeric array whose positions mean nothing to a reader.
json_threshold <- function(x) {
  if (is.null(names(x))) return(x)
  as.list(x)
}

json_policy <- function(policy) {
  if (is.null(policy)) return(NULL)
  fields <- unclass(policy)
  # `lapply()` keeps NULL-valued members, whereas `fields[[name]] <- NULL`
  # would delete them and the artifact would no longer list every threshold.
  lapply(rlang::set_names(names(fields)), function(field) {
    value <- fields[[field]]
    switch(
      field,
      required_probes = json_array(value),
      min_recall = ,
      min_precision = json_threshold(value),
      value
    )
  })
}

json_calibration <- function(x) {
  list(
    n_input = x$n_input,
    n_items = x$n_items,
    n_excluded = x$n_excluded,
    n_consensus_ties = x$n_consensus_ties,
    n_insufficient_human_ratings = x$n_insufficient_human_ratings,
    n_judge_abstentions = x$n_judge_abstentions,
    judge_abstention_rate = x$judge_abstention_rate,
    judge_abstention_conf_low = x$judge_abstention_conf_low,
    judge_abstention_conf_high = x$judge_abstention_conf_high,
    label_levels = json_array(x$label_levels),
    scale = x$scale,
    weights = x$weights,
    conf_level = x$conf_level,
    n_boot = x$n_boot,
    seed = x$seed,
    coefficients = json_array(x$coefficients),
    agreement = as.data.frame(x$agreement),
    confusion = json_confusion(x$confusion),
    confusion_metrics = as.data.frame(x$confusion_metrics),
    human_reliability = if (is.null(x$human_reliability)) NULL else
      as.data.frame(x$human_reliability),
    per_rater_agreement = if (is.null(x$per_rater_agreement)) NULL else
      as.data.frame(x$per_rater_agreement)
  )
}

# The long-form counts, one row per reference-by-judge cell including empty
# cells, without the R-side attributes a `yourhonor_confusion` carries.
json_confusion <- function(confusion) {
  if (is.null(confusion)) return(NULL)
  out <- as.data.frame(unclass(confusion), stringsAsFactors = FALSE)
  attr(out, "label_levels") <- NULL
  attr(out, "metrics") <- NULL
  out
}

json_bias <- function(bias) {
  if (is.null(bias)) return(NULL)
  list(
    audit_id = audit_id(bias),
    results = as.data.frame(bias)
  )
}

json_stability <- function(stability) {
  if (is.null(stability)) return(NULL)
  list(
    audit_id = audit_id(stability),
    n_items = stability$n_items,
    n_items_scored = stability$n_items_scored,
    n_runs = stability$n_runs,
    label_levels = json_array(stability$label_levels),
    weights = stability$weights,
    conf_level = stability$conf_level,
    n_boot = stability$n_boot,
    pairwise_n_boot_effective = stability$pairwise_n_boot_effective,
    stability_n_boot_effective = stability$stability_n_boot_effective,
    n_boot_effective = stability$n_boot_effective,
    pairwise_disagreement = stability$pairwise_disagreement,
    pairwise_disagreement_conf_low = stability$pairwise_disagreement_conf_low,
    pairwise_disagreement_conf_high = stability$pairwise_disagreement_conf_high,
    percent_agreement = stability$percent_agreement,
    any_flip_rate_at_n_runs = stability$any_flip_rate_at_n_runs,
    n_items_complete = stability[["n_items_complete"]],
    pairwise_disagreement_worst_case =
      stability[["pairwise_disagreement_worst_case"]],
    pairwise_disagreement_worst_case_high =
      stability[["pairwise_disagreement_worst_case_high"]],
    any_flip_rate_worst_case = stability[["any_flip_rate_worst_case"]],
    pairwise_disagreement_best_case =
      stability[["pairwise_disagreement_best_case"]],
    pairwise_disagreement_best_case_low =
      stability[["pairwise_disagreement_best_case_low"]],
    gwet_agreement = stability$stability,
    gwet_coefficient = stability$stability_coefficient,
    gwet_conf_low = stability$stability_conf_low,
    gwet_conf_high = stability$stability_conf_high,
    gwet_status = stability$stability_status,
    gwet_boundary_degenerate = stability$stability_boundary_degenerate,
    missing_rate = stability$missing_rate,
    pairwise_ci_reliable = stability$pairwise_ci_reliable,
    stability_ci_reliable = stability$stability_ci_reliable,
    ci_reliable = stability$ci_reliable,
    provenance = json_provenance(stability$provenance)
  )
}

# Only the adapter-written provenance fields are known to be vectors; caller
# supplied provenance is serialized verbatim, as documented.
json_provenance <- function(provenance) {
  if (is.null(provenance)) return(NULL)
  array_fields <- c("input_cols", "human_cols", "label_levels", "content_cols")
  for (field in intersect(array_fields, names(provenance))) {
    # `[<-` with a list keeps a NULL member; `[[<-` NULL would delete it.
    provenance[field] <- list(json_array(provenance[[field]]))
  }
  provenance
}
