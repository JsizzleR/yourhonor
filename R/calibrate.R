#' Calibrate an LLM-as-judge against human labels
#'
#' Runs a judge on a redacted view of a human-labeled set and reports agreement,
#' confusion metrics, and (for multiple human raters) human reliability. The
#' result is descriptive evidence; deployment decisions require an explicit
#' [acceptance_policy()].
#'
#' @section The judge contract:
#' When `judge` is supplied, `input_cols` is required. The judge receives only a
#' generated `.item_id` plus those columns; human labels and existing judge
#' labels are never exposed. It may return either one label per input row or a
#' data frame with `.item_id` and `label` columns. Structured output is preferred
#' because IDs make accidental reordering detectable.
#'
#' Items are shuffled before grading and the `.item_id` the judge sees is
#' assigned after the shuffle, so it is not the source row number. The result
#' keeps both keys: `items$.item_id` is the source-row audit key and
#' `items$.judge_item_id` is the key the judge saw. Join judge-side logs
#' (prompts, raw completions) on `.judge_item_id`, never on `.item_id`.
#'
#' @section Human consensus:
#' With one column in `human_cols` the human label is used as given. With
#' several, each item's reference label is the modal (most frequent) human
#' label. An item with fewer than two observed human ratings has no consensus
#' and is counted in `n_insufficient_human_ratings`; an item whose most
#' frequent labels tie has no consensus either, is flagged in
#' `items$consensus_tie`, and is counted in `n_consensus_ties`. Items without a
#' consensus are excluded from the judge-versus-consensus agreement and
#' confusion evidence (`n_excluded`). Because ties are exactly the items humans
#' found hardest, and with two raters every human disagreement is a tie, the
#' judge is scored only on the items humans agreed about; that direction is
#' optimistic. `per_rater_agreement` compares the judge against each rater on
#' every item and is the all-items counterpart.
#'
#' @param gold A data frame of labeled examples.
#' @param judge A function receiving the redacted judge input, or `NULL` to read
#'   precomputed labels from `judge_col`.
#' @param input_cols Columns visible to `judge`. Required when `judge` is a
#'   function. Human columns and `judge_col` are forbidden.
#' @param id_col Optional unique item identifier. A sequential internal ID is
#'   generated when omitted.
#' @param human_cols Human-rater columns. Supplying more than one enables the
#'   modal-consensus rule described in the "Human consensus" section and the
#'   human-reliability benchmark.
#' @param judge_col Column containing precomputed judge labels when `judge` is
#'   `NULL`.
#' @param label_levels Complete label universe in intended order. Labels are
#'   matched exactly (case and whitespace are significant) and empty labels are
#'   rejected; represent a failed judgement as `NA`. Required for
#'   `scale = "ordinal"`.
#' @param scale Whether labels are nominal or ordinal.
#' @param weights Agreement weights. `NULL` selects `"unweighted"` for nominal
#'   labels and `"ordinal"` for ordinal labels. Weighted coefficients use
#'   Gwet's weight schemes; see [agreement()].
#' @param coefficients Passed to [agreement()]. `NULL` (the default) selects
#'   Cohen's kappa, Krippendorff's alpha, and Gwet's AC1, with AC2 replacing
#'   AC1 for weighted (ordinal) runs.
#' @param conf_level,n_boot,seed Passed to [agreement()].
#' @param provenance Optional named list describing the judge model, prompt,
#'   parameters, and other audit metadata.
#' @param audit_id Optional identifier for the frozen judge, prompt, and dataset
#'   snapshot. Use the same non-empty value for related bias and stability
#'   evidence so [report()] can prevent accidental cross-run bundling.
#'
#' @return A `yourhonor_calibration` object with agreement, confusion and
#'   per-class metrics, human reliability, judge-versus-each-rater agreement,
#'   item-level evidence, label metadata, and provenance. `human_ceiling` remains
#'   as a compatibility alias for `human_reliability` but is no longer described
#'   as an upper bound.
#'
#'   Item-level elements:
#'   * `items`: `gold` plus `.item_id` (source-row key), `.judge_item_id`
#'     (the shuffled key the judge saw; `NA` when `judge = NULL`), the judge
#'     label, `agree`, and for several raters `human_consensus`,
#'     `n_human_observed`, and `consensus_tie`.
#'   * `judge_output`: the judge's own returned rows in the order it saw them,
#'     keyed by `.judge_item_id` with `.item_id` joined in; `NULL` when
#'     `judge = NULL`.
#'   * `blinding_map`: one row per item mapping `.judge_item_id` to
#'     `.item_id`; `NULL` when `judge = NULL`.
#'
#'   Exclusion counts: `n_input` items were supplied, `n_items` had a usable
#'   human consensus and judge label, `n_excluded = n_input - n_items`, and
#'   `n_consensus_ties` and `n_insufficient_human_ratings` break the exclusions
#'   down for multi-rater runs.
#' @export
#' @examples
#' gold <- tibble::tibble(
#'   input = paste("answer", 1:8),
#'   human_label = c("C", "C", "P", "I", "C", "P", "I", "C"),
#'   judge_label = c("C", "C", "C", "I", "C", "P", "P", "C")
#' )
#' calibrate_judge(gold, label_levels = c("I", "P", "C"),
#'                 n_boot = 200, seed = 1)
calibrate_judge <- function(gold,
                            judge = NULL,
                            input_cols = NULL,
                            id_col = NULL,
                            human_cols = "human_label",
                            judge_col = "judge_label",
                            label_levels = NULL,
                            scale = c("nominal", "ordinal"),
                            weights = NULL,
                            coefficients = NULL,
                            conf_level = 0.95,
                            n_boot = 2000,
                            seed = NULL,
                            provenance = list(),
                            audit_id = NULL) {
  gold <- tibble::as_tibble(gold)
  if (".item_id" %in% names(gold)) {
    cli::cli_abort("{.field .item_id} is reserved for opaque internal audit keys.")
  }
  scale <- rlang::arg_match(scale)
  conf_level <- check_probability(conf_level, open = TRUE)
  n_boot <- check_count(n_boot)
  audit_id <- check_audit_id(audit_id)
  if (!is.list(provenance) || (length(provenance) > 0L && is.null(names(provenance)))) {
    cli::cli_abort("{.arg provenance} must be a named list (an empty list is allowed).")
  }

  human_cols <- check_column_names(human_cols)
  judge_col <- check_column_name(judge_col)
  if (judge_col %in% human_cols) {
    cli::cli_abort("{.arg judge_col} must differ from every {.arg human_cols} entry.")
  }
  missing_humans <- setdiff(human_cols, names(gold))
  if (length(missing_humans) > 0L) {
    cli::cli_abort(
      "{.arg gold} is missing human rater column{?s}: {.field {missing_humans}}."
    )
  }
  human_source <- gold[human_cols]
  if (scale == "ordinal" && is.null(label_levels)) {
    cli::cli_abort("{.arg label_levels} is required when {.code scale = 'ordinal'}.")
  }
  label_levels <- resolve_label_levels(human_source, label_levels, arg = "human_cols")
  if (length(label_levels) == 0L) {
    cli::cli_abort("Human ratings contain no observed labels.")
  }
  if (is.null(weights)) weights <- if (scale == "ordinal") "ordinal" else "unweighted"
  weights <- rlang::arg_match(weights, c("unweighted", "linear", "quadratic", "ordinal"))
  if (scale == "nominal" && weights != "unweighted") {
    cli::cli_abort("Weighted agreement requires {.code scale = 'ordinal'}.")
  }
  coefficients <- resolve_coefficients(coefficients, weights)
  if (!is.null(seed)) withr::local_seed(seed)

  item_id <- calibration_ids(gold, id_col)
  graded <- grade_calibration(
    judge = judge,
    gold = gold,
    input_cols = input_cols,
    protected_cols = unique(c(human_cols, judge_col)),
    judge_col = judge_col,
    label_levels = label_levels
  )
  judge_label <- graded$label

  human_mat <- as.data.frame(lapply(human_source, as.character), stringsAsFactors = FALSE)
  human_observed <- rowSums(!is.na(human_mat))
  human <- if (length(human_cols) > 1L) {
    out <- apply(human_mat, 1, modal_value)
    out[human_observed < 2L] <- NA_character_
    out
  } else {
    human_mat[[1]]
  }
  consensus_tie <- if (length(human_cols) > 1L) {
    out <- apply(human_mat, 1, is_tie)
    out[human_observed < 2L] <- FALSE
    out
  } else {
    rep(FALSE, nrow(gold))
  }

  ratings_full <- data.frame(
    human = as.character(human),
    judge = as.character(judge_label),
    stringsAsFactors = FALSE
  )
  complete <- stats::complete.cases(ratings_full)
  if (sum(complete) < 2L) {
    cli::cli_abort("Need at least 2 complete judge-human pairs to calibrate.")
  }
  if (any(!complete)) {
    cli::cli_warn(
      "{sum(!complete)} item{?s} lacked a usable consensus or judge label; exclusions are recorded."
    )
  }

  agr <- compute_agreement(
    ratings_full, coefficients, conf_level, n_boot, seed,
    label_levels = label_levels, weights = weights
  )
  conf <- confusion(
    ratings_full, label_levels = label_levels, conf_level = conf_level,
    human_col = "human", judge_col = "judge"
  )
  conf_metrics <- confusion_metrics(conf)

  human_reliability <- NULL
  per_rater <- NULL
  if (length(human_cols) > 1L) {
    human_coefficients <- resolve_gwet_coefficient(
      multirater_coefficients, weights, auto = TRUE
    )
    human_reliability <- compute_agreement(
      human_mat, human_coefficients, conf_level, n_boot, seed,
      label_levels = label_levels, weights = weights
    )
    per_rater <- do.call(vctrs::vec_rbind, lapply(seq_along(human_cols), function(i) {
      pair <- data.frame(human = human_mat[[i]], judge = judge_label)
      out <- compute_agreement(
        pair, coefficients, conf_level, n_boot, seed,
        label_levels = label_levels, weights = weights
      )
      out$rater <- human_cols[[i]]
      out[, c("rater", setdiff(names(out), "rater"))]
    }))
  }

  blinding_map <- calibration_blinding_map(graded$blinding_map, item_id)
  items <- gold
  items$.item_id <- item_id
  items$.judge_item_id <- if (is.null(blinding_map)) {
    NA_integer_
  } else {
    blinding_map$.judge_item_id[match(item_id, blinding_map$.item_id)]
  }
  items[[judge_col]] <- judge_label
  if (length(human_cols) > 1L) {
    items$human_consensus <- human
    items$n_human_observed <- human_observed
    items$consensus_tie <- consensus_tie
  }
  items$agree <- ifelse(complete, ratings_full$human == ratings_full$judge, NA)
  items <- tibble::as_tibble(items)

  provenance <- utils::modifyList(
    provenance,
    list(
      created_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
      input_cols = if (is.null(judge)) character(0) else input_cols,
      id_col = id_col,
      judge_col = judge_col,
      human_cols = human_cols,
      label_levels = label_levels,
      scale = scale,
      weights = weights,
      audit_id = audit_id
    )
  )

  structure(
    list(
      agreement = agr,
      confusion = conf,
      confusion_metrics = conf_metrics,
      human_reliability = human_reliability,
      human_ceiling = human_reliability,
      per_rater_agreement = per_rater,
      items = items,
      ratings = ratings_full,
      judge_output = judge_output_table(graded$visible_output, blinding_map),
      blinding_map = blinding_map,
      coefficients = coefficients,
      conf_level = conf_level,
      n_boot = n_boot,
      seed = seed,
      n_items = sum(complete),
      n_input = nrow(gold),
      n_excluded = sum(!complete),
      n_consensus_ties = sum(consensus_tie),
      n_insufficient_human_ratings = if (length(human_cols) > 1L) {
        sum(human_observed < 2L)
      } else {
        0L
      },
      human_cols = human_cols,
      judge_col = judge_col,
      input_cols = input_cols,
      id_col = id_col,
      label_levels = label_levels,
      scale = scale,
      weights = weights,
      audit_id = audit_id,
      provenance = provenance
    ),
    class = "yourhonor_calibration"
  )
}

#' @export
print.yourhonor_calibration <- function(x, ...) {
  cli::cli_h1("yourhonor judge calibration")
  cli::cli_text(
    "{x$n_items} usable item{?s} of {x$n_input} - descriptive judge-vs-human agreement:"
  )
  cli::cli_ul()
  for (i in seq_len(nrow(x$agreement))) {
    ag <- x$agreement[i, ]
    if (ag$status == "ok") {
      # One string: `cli_li()`'s second positional argument is `labels`, so
      # a second fragment would be swallowed rather than printed.
      cli::cli_li(paste0(
        "{.strong {ag$coefficient}}: {fmt_num(ag$estimate)} ",
        "[{fmt_num(ag$conf_low)}, {fmt_num(ag$conf_high)}] ({ag$interpretation})"
      ))
    } else {
      cli::cli_li("{.strong {ag$coefficient}}: not estimable ({ag$status})")
    }
  }
  cli::cli_end()
  n_disagree <- sum(!x$items$agree, na.rm = TRUE)
  cli::cli_text("{n_disagree} disagreement{?s}; inspect {.fn confusion_metrics} before use.")
  if (!is.null(x$human_reliability)) {
    gwet <- x$human_reliability[
      x$human_reliability$coefficient %in% c("gwet_ac2", "gwet_ac1"), , drop = FALSE
    ]
    if (nrow(gwet) > 0L) {
      cli::cli_text(
        "Human reliability benchmark ({length(x$human_cols)} raters): ",
        "{gwet$coefficient[[1]]} = {fmt_num(gwet$estimate[[1]])}."
      )
    }
  }
  if (x$n_consensus_ties > 0L) {
    cli::cli_text("{x$n_consensus_ties} consensus tie{?s} recorded explicitly.")
  }
  if (x$n_insufficient_human_ratings > 0L) {
    cli::cli_text(
      "{x$n_insufficient_human_ratings} item{?s} had fewer than two observed human ratings."
    )
  }
  if (x$n_excluded > 0L) {
    cli::cli_text("{x$n_excluded} item{?s} excluded from judge-consensus agreement.")
  }
  cli::cli_alert_info("No deployment decision is implied; call report() with a policy to assess one.")
  invisible(x)
}

calibration_ids <- function(gold, id_col, call = rlang::caller_env()) {
  validate_source_ids(gold, id_col, "gold", call = call)
  # Never expose caller IDs implicitly: they can contain labels or personal
  # information. `.item_id` is the source-row audit key kept on the result;
  # the key the judge sees is assigned separately after shuffling (see
  # `grade_blind()`) and is returned as `.judge_item_id`.
  seq_len(nrow(gold))
}

# Translate the judge-side key back to the source-row audit key.
calibration_blinding_map <- function(blinding_map, item_id) {
  if (is.null(blinding_map)) return(NULL)
  tibble::tibble(
    .judge_item_id = blinding_map$.item_id,
    .item_id = item_id[blinding_map$.source_row]
  )
}

# The judge's rows carry the key it saw as `.item_id`; rename it so the table
# can be joined to `items` without confusing the two key spaces.
judge_output_table <- function(visible_output, blinding_map) {
  if (is.null(visible_output)) return(NULL)
  out <- visible_output
  names(out)[names(out) == ".item_id"] <- ".judge_item_id"
  out$.item_id <- blinding_map$.item_id[
    match(out$.judge_item_id, blinding_map$.judge_item_id)
  ]
  keys <- c(".judge_item_id", ".item_id")
  out[, c(keys, setdiff(names(out), keys))]
}

grade_calibration <- function(judge,
                              gold,
                              input_cols,
                              protected_cols,
                              judge_col,
                              label_levels,
                              call = rlang::caller_env()) {
  if (is.null(judge)) {
    if (!judge_col %in% names(gold)) {
      cli::cli_abort(
        paste0(
          "With {.code judge = NULL}, {.arg gold} must contain ",
          "{.field {judge_col}}."
        ),
        call = call
      )
    }
    labels <- validate_labels(
      gold[[judge_col]], label_levels, arg = judge_col, call = call
    )
    return(list(
      label = labels, output = NULL, visible_output = NULL, blinding_map = NULL
    ))
  }
  if (!is.function(judge)) {
    cli::cli_abort(
      "{.arg judge} must be a function or {.code NULL}.",
      call = call
    )
  }
  judge_input <- redacted_judge_input(
    gold, input_cols, protected_cols = protected_cols, data_arg = "gold",
    call = call
  )
  grade_blind(judge, judge_input, label_levels = label_levels, call = call)
}

# Validate, but never implicitly reveal, caller-owned identifiers.
validate_source_ids <- function(data,
                                id_col,
                                data_arg = "data",
                                call = rlang::caller_env()) {
  if (is.null(id_col)) return(invisible(NULL))
  id_col <- check_column_name(id_col, "id_col", call = call)
  if (!id_col %in% names(data)) {
    cli::cli_abort(
      "{.arg id_col} must name one column in {.arg {data_arg}}.",
      call = call
    )
  }
  ids <- data[[id_col]]
  if (anyNA(ids) || anyDuplicated(ids)) {
    cli::cli_abort(
      "{.arg id_col} must contain unique, non-missing IDs.",
      call = call
    )
  }
  invisible(NULL)
}

redacted_judge_input <- function(data,
                                 input_cols,
                                 id_col = NULL,
                                 protected_cols = character(0),
                                 data_arg = "data",
                                 call = rlang::caller_env()) {
  data <- tibble::as_tibble(data)
  validate_source_ids(data, id_col, data_arg, call = call)
  if (is.null(input_cols) || length(input_cols) == 0L) {
    cli::cli_abort(
      "{.arg input_cols} is required when {.arg judge} is supplied.",
      call = call
    )
  }
  input_cols <- check_column_names(input_cols, "input_cols", call = call)
  if (".item_id" %in% input_cols) {
    cli::cli_abort(
      "{.field .item_id} is reserved and cannot appear in {.arg input_cols}.",
      call = call
    )
  }
  missing_inputs <- setdiff(input_cols, names(data))
  if (length(missing_inputs) > 0L) {
    cli::cli_abort(
      "Unknown {.arg input_cols}: {.field {missing_inputs}}.",
      call = call
    )
  }
  leaked <- intersect(input_cols, protected_cols)
  if (length(leaked) > 0L) {
    cli::cli_abort(
      "Protected label column{?s} cannot be judge inputs: {.field {leaked}}.",
      call = call
    )
  }
  tibble::as_tibble(data[input_cols])
}

# Shuffle first, then assign fresh display keys. This prevents source order and
# key ranges from becoming side channels for hidden conditions or labels.
# `blinding_map` records which source row each display key came from so the
# judge's own output can be attributed after the fact.
grade_blind <- function(judge,
                        data,
                        label_levels = NULL,
                        context = "Structured judge output",
                        levels_arg = "label_levels",
                        call = rlang::caller_env()) {
  data <- sanitize_judge_frame(data)
  if (".item_id" %in% names(data)) {
    cli::cli_abort(
      "Internal blind-grading input cannot already contain {.field .item_id}.",
      call = call
    )
  }
  permutation <- sample.int(nrow(data))
  visible <- data[permutation, , drop = FALSE]
  visible <- tibble::add_column(
    visible, .item_id = seq_len(nrow(visible)), .before = 1L
  )
  graded <- grade_keyed(
    judge, visible, label_levels = label_levels,
    key_cols = ".item_id", context = context, levels_arg = levels_arg,
    call = call
  )
  restored <- character(nrow(data))
  restored[permutation] <- graded$label
  list(
    label = restored,
    output = tibble::tibble(.item_id = seq_len(nrow(data)), label = restored),
    visible_output = graded$output,
    blinding_map = tibble::tibble(
      .item_id = seq_len(nrow(data)), .source_row = permutation
    )
  )
}

# Element names and free-form attributes travel with the values through a
# shuffle, so a column built with sapply() over labels or IDs would hand the
# judge every human label row-aligned. Keep only the attributes a vector needs
# to keep its class, and rebuild the frame so table-level attributes are gone.
sanitize_judge_frame <- function(data) {
  data <- tibble::as_tibble(data, .name_repair = "minimal")
  columns <- lapply(data, sanitize_judge_value)
  tibble::new_tibble(columns, nrow = nrow(data))
}

# Names on a column (or on any atomic vector inside it) are row-aligned and
# are dropped; names inside nested lists are keys and are kept.
sanitize_judge_value <- function(x, nested = FALSE) {
  # Objects without row-aligned names are passed through untouched.
  if (isS4(x) || is.environment(x) || is.function(x)) return(x)
  if (is.data.frame(x)) return(sanitize_judge_frame(x))
  # POSIXlt keeps every datetime component as a whole vector on each element.
  if (inherits(x, "POSIXlt")) x <- as.POSIXlt(unname(as.POSIXct(x)))
  # A record is a named list of fields; the fields are the column's data.
  # Scalar attributes are class metadata (a precision, a unit) and stay;
  # anything longer could be row-aligned and is dropped.
  if (inherits(x, "vctrs_rcrd")) {
    fields <- vctrs::fields(x)
    attrs <- attributes(x)
    extra <- setdiff(names(attrs)[lengths(attrs) > 1L], c("names", "class"))
    for (name in extra) attr(x, name) <- NULL
    for (field in fields) {
      vctrs::field(x, field) <- sanitize_judge_value(vctrs::field(x, field))
    }
    return(x)
  }
  if (!nested || !vctrs::obj_is_list(x)) x <- vctrs::vec_set_names(x, NULL)
  keep <- c("names", "class", "levels", "tzone", "units", "dim", "dimnames", "ptype")
  extra <- setdiff(names(attributes(x)), keep)
  for (name in extra) attr(x, name) <- NULL
  # Bare lists, list_of and AsIs columns hold one value per row and are
  # traversed; other classed list-likes own their components and are not.
  if (vctrs::obj_is_list(x)) {
    x[] <- lapply(x, sanitize_judge_value, nested = TRUE)
  }
  x
}

grade_keyed <- function(judge,
                        data,
                        label_levels = NULL,
                        key_cols = ".item_id",
                        context = "Structured judge output",
                        levels_arg = "label_levels",
                        call = rlang::caller_env()) {
  if (!is.function(judge)) {
    cli::cli_abort("{.arg judge} must be a function.", call = call)
  }
  data <- tibble::as_tibble(data)
  if (!all(key_cols %in% names(data)) || anyNA(data[key_cols]) ||
      anyDuplicated(as.data.frame(data[key_cols]))) {
    cli::cli_abort(
      "Internal judge keys must be present, unique, and non-missing.",
      call = call
    )
  }
  out <- judge(data)
  if (is.data.frame(out)) {
    required <- c(key_cols, "label")
    if (!all(required %in% names(out))) {
      cli::cli_abort(
        paste0(
          "{context} must contain {.field label} and key column{?s}: ",
          "{.field {key_cols}}."
        ),
        call = call
      )
    }
    output <- tibble::as_tibble(out)
    output_keys <- as.data.frame(output[key_cols])
    input_keys <- as.data.frame(data[key_cols])
    index <- vctrs::vec_match(input_keys, output_keys)
    reverse <- vctrs::vec_match(output_keys, input_keys)
    if (nrow(output) != nrow(data) || anyNA(output_keys) ||
        anyDuplicated(output_keys) || anyNA(index) || anyNA(reverse)) {
      cli::cli_abort(
        "{context} must return every input key exactly once.",
        call = call
      )
    }
    labels <- output$label[index]
  } else {
    if (length(out) != nrow(data)) {
      cli::cli_abort(
        paste0(
          "The judge returned {length(out)} label{?s} for {nrow(data)} row{?s}; ",
          "it must return one label per row."
        ),
        call = call
      )
    }
    labels <- out
    output <- tibble::as_tibble(data[key_cols])
    output$label <- as.character(out)
  }
  labels <- as.character(labels)
  check_labels_nonblank(labels, "judge", call = call)
  if (!is.null(label_levels)) {
    labels <- validate_labels(
      labels, label_levels, arg = "judge", levels_arg = levels_arg, call = call
    )
  }
  list(label = labels, output = output)
}
