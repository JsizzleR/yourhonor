# Shared fixtures for the report and acceptance-policy tests.

make_cal <- function(n_boot = 100, conf_level = 0.95) {
  human <- rep(c("I", "P", "C"), each = 100)
  judge <- human
  judge[c(1, 101, 201)] <- c("P", "C", "P")
  gold <- tibble::tibble(
    human_label = human,
    judge_label = judge
  )
  calibrate_judge(
    gold, label_levels = c("I", "P", "C"),
    coefficients = "gwet_ac1", conf_level = conf_level,
    n_boot = n_boot, seed = 1,
    audit_id = "test-bundle"
  )
}

# Dependent thresholds travel with their gate: they are supplied only when
# the gate is on, exactly as acceptance_policy() requires.
make_policy <- function(required_probes = character(0),
                        max_pairwise_disagreement = NULL,
                        min_human_reliability_lower = NULL,
                        min_precision = NULL,
                        min_probe_n = 20,
                        conf_level = 0.95) {
  args <- list(
    min_agreement_lower = 0.9,
    min_n = 50,
    min_class_n = 15,
    min_recall = 0.8,
    max_missing_rate = 0,
    max_pairwise_disagreement = max_pairwise_disagreement,
    required_probes = required_probes,
    min_human_reliability_lower = min_human_reliability_lower,
    min_precision = min_precision,
    conf_level = conf_level,
    coefficient = "gwet_ac1"
  )
  if (!is.null(max_pairwise_disagreement)) {
    args <- c(args, list(
      min_stability_n = 50, min_stability_runs = 3,
      max_stability_missing_rate = 0
    ))
  }
  if (length(required_probes) > 0L) {
    args <- c(args, list(
      min_probe_n = min_probe_n, max_probe_flip_rate = 0.1,
      max_probe_directional_rate = 0.1, max_probe_missing_rate = 0
    ))
  }
  do.call(acceptance_policy, args)
}

flat_judge <- function(d) rep("C", nrow(d))

length_judge <- function(d) ifelse(nchar(d$input) > 40, "C", "P")

make_bias <- function(judge = flat_judge, probes = "verbosity", n = 60,
                      audit_id = "test-bundle") {
  probe_bias(
    tibble::tibble(input = paste("answer", seq_len(n))), judge,
    probes = probes, seed = 1, audit_id = audit_id
  )
}

make_stability <- function(judge = flat_judge, n = 60,
                           audit_id = "test-bundle") {
  test_retest(
    tibble::tibble(input = paste("answer", seq_len(n))), judge,
    input_cols = "input", n_runs = 3, label_levels = c("I", "P", "C"),
    n_boot = 50, seed = 1, audit_id = audit_id
  )
}

check_status <- function(rep, check) {
  rep$checks$status[rep$checks$check == check]
}

tie_pairs <- function(n) {
  good_first <- seq_len(n) %% 2L == 0L
  tibble::tibble(
    context = paste("question", seq_len(n)),
    candidate_a = ifelse(good_first, "good answer", "bad answer"),
    candidate_b = ifelse(good_first, "bad answer", "good answer")
  )
}
