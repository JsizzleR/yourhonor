# Position probe with repeats >= 2: scored items with missing repeats.

# A judge that answers each (pair, display order) from a script, one answer
# per call in call order; NA abstains. Repeats of one order are exchangeable
# for the modal choice, so the order the calls arrive in does not matter.
scripted_judge <- function(script) {
  seen <- new.env()
  function(d) {
    vapply(seq_len(nrow(d)), function(i) {
      original <- startsWith(d$.candidate_1[[i]], "a")
      shown_first <- d$.candidate_1[[i]]
      item <- as.integer(sub("^[ab]", "", shown_first))
      condition <- if (original) "original" else "swapped"
      key <- paste(item, condition)
      k <- (if (is.null(seen[[key]])) 0L else seen[[key]]) + 1L
      seen[[key]] <- k
      script(item, condition)[[k]]
    }, character(1))
  }
}

scripted_position <- function(n, script, repeats = 3, tie_label = NULL) {
  pairs <- tibble::tibble(a = paste0("a", seq_len(n)), b = paste0("b", seq_len(n)))
  probe_position(
    pairs, scripted_judge(script), "a", "b", repeats = repeats,
    tie_label = tie_label, seed = 1, audit_id = "test-bundle"
  )
}

position_policy <- function(flip, direction, missing) {
  acceptance_policy(
    min_agreement_lower = 0.9, min_n = 50, min_class_n = 15,
    min_recall = 0.8, max_missing_rate = 0,
    max_pairwise_disagreement = NULL,
    required_probes = "position", min_probe_n = 10,
    max_probe_flip_rate = flip, max_probe_directional_rate = direction,
    max_probe_missing_rate = missing, coefficient = "gwet_ac1"
  )
}

position_gates <- function(bias, policy) {
  rows <- single_probe_checks("position", make_cal(), bias, policy)
  stats::setNames(rows, vapply(rows, `[[`, "", "check"))
}

gate_status <- function(rows) {
  c(
    flip = rows$bias_position$status,
    direction = rows$bias_position_direction$status
  )
}

# Items 1-10 keep their choice, 11-20 always take the first slot, and 21-30
# take it too but abstain on two of three original-order repeats; answered
# as B, B those repeats would make the item unflipped.
fail_side_script <- function(abstain) {
  function(item, condition) {
    if (item <= 10L) {
      return(if (condition == "original") rep("A", 3) else rep("B", 3))
    }
    if (item <= 20L || condition == "swapped") return(rep("A", 3))
    if (abstain) c("A", NA, NA) else c("A", "B", "B")
  }
}

test_that("abstaining on repeats of items that stay scored cannot fail a position probe", {
  policy <- position_policy(flip = 0.3, direction = 0.3, missing = 0.15)
  abstained <- scripted_position(30, fail_side_script(abstain = TRUE))
  completed <- scripted_position(30, fail_side_script(abstain = FALSE))
  # The abstaining items stay scored, as flips.
  expect_identical(abstained$n_scored, 30L)
  expect_identical(abstained$n_discordant, 20L)
  expect_identical(abstained$n_unscored, 0L)
  expect_gt(abstained$conf_low, 0.3)
  expect_identical(
    gate_status(position_gates(completed, policy)),
    c(flip = "INDETERMINATE", direction = "INDETERMINATE")
  )
  rows <- position_gates(abstained, policy)
  expect_identical(
    gate_status(rows), c(flip = "INDETERMINATE", direction = "INDETERMINATE")
  )
  expect_match(
    rows$bias_position$reason,
    paste(
      "The lower bound counts the 10 scored items whose missing repeats",
      "could change their outcome, as unflipped over 30 items."
    ),
    fixed = TRUE
  )
  expect_match(
    rows$bias_position$reason,
    paste(
      "The upper bound counts the 10 scored items whose missing repeats",
      "could change their outcome, as flips over all 30 items."
    ),
    fixed = TRUE
  )
})

# 100 pairs: the original order always picks candidate a; the swapped order
# picks it once and the first slot twice (a flip), or abstains on those two.
pass_side_script <- function(abstain) {
  function(item, condition) {
    if (condition == "original") return(rep("A", 3))
    if (abstain) c("B", NA, NA) else c("B", "A", "A")
  }
}

test_that("abstaining on the repeats that would flip cannot pass a position probe", {
  policy <- position_policy(flip = 0.2, direction = 0.2, missing = 0.35)
  complete <- scripted_position(100, pass_side_script(abstain = FALSE))
  abstained <- scripted_position(100, pass_side_script(abstain = TRUE))
  expect_equal(abstained$missing_rate, 1 / 3)
  expect_identical(abstained$n_discordant, 0L)
  # Positive control: complete, the biased judge fails.
  expect_identical(
    gate_status(position_gates(complete, policy)),
    c(flip = "FAIL", direction = "FAIL")
  )
  rows <- position_gates(abstained, policy)
  expect_identical(rows$bias_position_missing_rate$status, "PASS")
  expect_identical(
    gate_status(rows), c(flip = "INDETERMINATE", direction = "INDETERMINATE")
  )
  expect_match(
    rows$bias_position_direction$reason,
    paste(
      "Each such scored item counts towards that direction only when its",
      "repeats can reach it or could leave it unscored, and as unflipped",
      "otherwise."
    ),
    fixed = TRUE
  )
})

# A position object built straight from display choices laid out as repeat x
# condition x item (original first), with its evidence attributes as
# probe_position() writes them.
position_from_display <- function(display, n, repeats, tie_label) {
  ev <- tibble::tibble(
    .item_id = rep(seq_len(n), times = 2L * repeats),
    .condition = rep(rep(c("original", "swapped"), each = n), times = repeats),
    .repeat = rep(seq_len(repeats), each = 2L * n),
    display_choice = display
  )
  ev$canonical_choice <- map_canonical_choice(
    ev$display_choice, ev$.condition, c("A", "B"), tie_label
  )
  item_ev <- position_item_evidence(ev, c("A", "B"))
  row <- bias_result_row(
    "position", position_stats(item_ev, ev, 0.95),
    conf_level = 0.95, alpha = 0.05, flip_threshold = NA_real_
  )
  attr(ev, "tie_label") <- if (is.null(tie_label)) NA_character_ else tie_label
  new_yourhonor_bias(
    finalize_bias_rows(row, "none"), audit_id = "test-bundle",
    evidence = list(position = ev), item_evidence = list(position = item_ev)
  )
}

# The interval the policy assesses: observed lower bound lowered by the best
# case, worst-case upper bound.
assessed_bounds <- function(bias) {
  unsettled <- position_row_unsettled(bias, bias)
  best <- best_case_lower_bounds(bias, unsettled)
  upper <- worst_case_upper_bounds(bias, unsettled)
  c(
    flip_low = min(bias$conf_low, best$flip),
    direction_low = min(bias$directional_conf_low, best$directional),
    flip_high = upper$flip, direction_high = upper$directional
  )
}

test_that("the bounds hold over every completion of the missing repeats", {
  set.seed(20261112)
  checked <- 0L
  for (case in 1:60) {
    repeats <- if (case %% 2L == 0L) 2L else 3L
    n <- sample(3:6, 1)
    tie_label <- if (case %% 3L == 0L) "T" else NULL
    answers <- c("A", "B", tie_label)
    cells <- 2L * n * repeats
    display <- ifelse(
      stats::runif(cells) < stats::runif(1), "A",
      sample(answers, cells, replace = TRUE)
    )
    gaps <- sample(cells, sample(1:3, 1))
    display[gaps] <- NA
    bias <- position_from_display(display, n, repeats, tie_label)
    if (bias$n_scored == 0L) next
    bounds <- assessed_bounds(bias)
    if (anyNA(bounds)) next
    completions <- as.matrix(expand.grid(
      rep(list(answers), length(gaps)), stringsAsFactors = FALSE
    ))
    for (k in seq_len(nrow(completions))) {
      filled <- display
      filled[gaps] <- completions[k, ]
      complete <- position_from_display(filled, n, repeats, tie_label)
      # A completion that scores nothing cannot fail or pass.
      if (complete$n_scored == 0L) {
        expect_lte(bounds[["flip_low"]], 0)
        next
      }
      expect_identical(position_row_unsettled(complete, complete), no_unsettled_items)
      upper <- worst_case_upper_bounds(complete)
      expect_lte(bounds[["flip_low"]], complete$conf_low + 1e-12)
      expect_lte(bounds[["direction_low"]], complete$directional_conf_low + 1e-12)
      expect_gte(bounds[["flip_high"]], upper$flip - 1e-12)
      expect_gte(bounds[["direction_high"]], upper$directional - 1e-12)
    }
    checked <- checked + 1L
  }
  expect_gt(checked, 40L)
})

test_that("a display order is completed only into modes its missing repeats can reach", {
  a <- "candidate_a"
  b <- "candidate_b"
  # Two of three already agree: no answer for the third changes the mode.
  expect_identical(reachable_modes(c(a, a, NA), tie_possible = TRUE), a)
  # One of three: the other candidate can win; a tie needs the tie label.
  expect_setequal(reachable_modes(c(a, NA, NA), tie_possible = FALSE), c(a, b))
  expect_setequal(
    reachable_modes(c(a, NA, NA), tie_possible = TRUE), c(a, b, "tie", NA)
  )
  # A split vote at repeats = 2 resolves either way, or stays tied.
  expect_setequal(reachable_modes(c(a, NA), tie_possible = FALSE), c(a, NA))
  expect_setequal(reachable_modes(c(NA, NA), tie_possible = FALSE), c(a, b, NA))
  # Nothing missing: the observed mode, tied or not.
  expect_identical(reachable_modes(c(a, b), tie_possible = TRUE), NA_character_)
})

# 270 always-first flips, and 30 items whose swapped order is B, NA, NA: with
# no tie label those can only stay unflipped or flip towards the first slot.
one_way_script <- function(item, condition) {
  if (item <= 270L || condition == "original") return(rep("A", 3))
  c("B", NA, NA)
}

test_that("missing repeats that can only add to the observed direction still fail", {
  policy <- position_policy(flip = 0.8, direction = 0.8, missing = 0.05)
  bias <- scripted_position(300, one_way_script)
  expect_identical(
    position_row_unsettled(bias, bias),
    utils::modifyList(no_unsettled_items, list(first = 30L, lower_first = 30L))
  )
  rows <- position_gates(bias, policy)
  expect_identical(gate_status(rows), c(flip = "FAIL", direction = "FAIL"))
  # Their best completion leaves them unflipped: the bound is the observed one.
  expect_identical(
    best_case_lower_bounds(bias, position_row_unsettled(bias, bias)),
    list(flip = bias$conf_low, directional = bias$directional_conf_low)
  )
})

test_that("evidence without a recorded tie label is completed with ties", {
  policy <- position_policy(flip = 0.8, direction = 0.8, missing = 0.05)
  bias <- scripted_position(300, one_way_script)
  evidence <- attr(bias, "evidence")
  expect_identical(attr(evidence$position, "tie_label"), NA_character_)
  attr(evidence$position, "tie_label") <- NULL
  attr(bias, "evidence") <- evidence
  # A tie answer could tie those items, so the upper bound counts them
  # either way; the lower bound still moves them only towards the first slot.
  expect_identical(
    position_row_unsettled(bias, bias),
    utils::modifyList(
      no_unsettled_items, list(free = 30L, lower_first = 30L, unscorable = 30L)
    )
  )
  rows <- position_gates(bias, policy)
  expect_identical(gate_status(rows), c(flip = "FAIL", direction = "FAIL"))
  tied <- scripted_position(5, one_way_script, tie_label = "T")
  expect_identical(attr(attr(tied, "evidence")$position, "tie_label"), "T")
})

test_that("without matching per-judgment evidence neither interval is assessed", {
  policy <- position_policy(flip = 0.3, direction = 0.3, missing = 0.15)
  bias <- scripted_position(30, fail_side_script(abstain = TRUE))
  stripped <- bias
  attr(stripped, "evidence") <- NULL
  edited <- bias
  edited$n_discordant <- 19L
  edited$statistic <- 19L
  edited$n_directional_items <- 19L
  for (x in list(stripped, edited)) {
    expect_null(position_row_unsettled(x, x))
    rows <- position_gates(x, policy)
    expect_identical(
      gate_status(rows), c(flip = "INDETERMINATE", direction = "INDETERMINATE")
    )
    expect_identical(rows$bias_position$value, NA_real_)
    expect_match(
      rows$bias_position$reason,
      "per-judgment evidence that would bound them is unavailable",
      fixed = TRUE
    )
  }
  # Combining keeps the evidence, so the bound is unchanged.
  pointwise <- make_bias()
  combined <- combine_bias(pointwise, bias)
  expect_identical(
    position_row_unsettled(combined[combined$probe == "position", ], combined),
    position_row_unsettled(bias, bias)
  )
})

test_that("with no scored item missing a repeat the row's own counts are used", {
  # Complete, with ties at repeats = 2.
  judge <- function(d) sample(c("A", "B"), nrow(d), replace = TRUE)
  complete <- probe_position(
    tie_pairs(120), judge, "candidate_a", "candidate_b",
    context_cols = "context", repeats = 2, seed = 3, audit_id = "test-bundle"
  )
  expect_identical(complete$missing_rate, 0)
  expect_gt(complete$n_unscored_tied, 0L)
  # Missing judgments only on items left unscored.
  only_unscored <- scripted_position(30, function(item, condition) {
    if (item <= 5L && condition == "original") return(rep(NA_character_, 3))
    rep("A", 3)
  })
  expect_identical(only_unscored$n_unscored, 5L)
  for (x in list(complete, only_unscored)) {
    stripped <- x
    attr(stripped, "evidence") <- NULL
    expect_identical(position_row_unsettled(stripped, stripped), no_unsettled_items)
    expect_identical(
      worst_case_upper_bounds(x)$flip,
      exact_binom_interval(x$n_discordant + x$n_unscored, x$n_input, 0.95)[[2]]
    )
    rows <- position_gates(x, position_policy(flip = 0.1, direction = 0.1, missing = 0.5))
    for (check in c("bias_position", "bias_position_direction")) {
      reason <- rows[[check]]$reason
      expect_match(reason, "The upper bound counts the [0-9]+ unscored items")
      expect_no_match(reason, "scored items whose|\\s$|  ")
    }
  }
})

test_that("an always-first judge missing single repeats at repeats = 2 still fails", {
  # 15 of 100 pairs miss one original-order repeat. Answered the other way
  # it would tie the item, never flip it towards the second slot.
  script <- function(item, condition) {
    if (item <= 15L && condition == "original") return(c("A", NA))
    rep("A", 2)
  }
  bias <- scripted_position(100, script, repeats = 2)
  expect_identical(bias$n_scored, 100L)
  expect_identical(bias$n_discordant, 100L)
  unsettled <- position_row_unsettled(bias, bias)
  expect_identical(
    unsettled,
    utils::modifyList(
      no_unsettled_items,
      list(
        free = 15L, lower_first = 15L, observed_first = 15L, unscorable = 15L
      )
    )
  )
  # Unflipped, they leave 85 of 100 flips towards the first slot.
  expect_identical(
    best_case_lower_bounds(bias, unsettled),
    list(
      flip = exact_binom_interval(85L, 100L, 0.95)[[1]],
      directional = directional_magnitude_interval(85L, 0L, 100L, 0.95)[[1]]
    )
  )
  rows <- position_gates(
    bias, position_policy(flip = 0.6, direction = 0.6, missing = 0.1)
  )
  expect_identical(gate_status(rows), c(flip = "FAIL", direction = "FAIL"))
})

test_that("one more unflipped item never raises a directional lower bound", {
  # The lower bound counts an item a completion would tie as unflipped, in
  # the denominator; that must never be above leaving it out.
  for (conf_level in c(0.8, 0.95)) {
    for (n in 2:40) {
      for (positive in 0:(n - 1)) {
        for (negative in 0:(n - 1 - positive)) {
          kept <- directional_magnitude_interval(positive, negative, n, conf_level)
          left_out <- directional_magnitude_interval(
            positive, negative, n - 1L, conf_level
          )
          if (kept[[1]] > left_out[[1]] + 1e-12) {
            fail(paste("raised at", positive, negative, n, conf_level))
          }
        }
      }
    }
  }
  succeed()
})

test_that("a tie in the evidence makes ties possible whatever its tie label says", {
  # Item 1 answers T once in the original order and misses the other repeat;
  # the swapped order takes the first slot twice. Its observed mode is the
  # tie, so a completion can keep it scored, and flipped, or leave it tied.
  n <- 10L
  display <- c(
    "T", rep("A", n - 1L), rep("A", n),
    NA, rep("A", n - 1L), rep("A", n)
  )
  bias <- position_from_display(display, n, repeats = 2L, tie_label = "T")
  expect_identical(bias$n_scored, n)
  evidence <- attr(bias, "evidence")
  attr(evidence$position, "tie_label") <- NA_character_
  attr(bias, "evidence") <- evidence
  unsettled <- position_row_unsettled(bias, bias)
  expect_identical(unsettled$free, 1L)
  expect_identical(unsettled_total(unsettled), 1L)
})

test_that("evidence with a list, matrix or raw column is not matched", {
  bias <- scripted_position(30, fail_side_script(abstain = TRUE))
  ids <- attr(bias, "evidence")$position$.item_id
  for (bad in list(
    lapply(ids, function(id) c(id, id)),
    lapply(ids, identity),
    matrix(ids, ncol = 1L),
    as.raw(match(ids, unique(ids)))
  )) {
    edited <- bias
    evidence <- attr(edited, "evidence")
    evidence$position$.item_id <- bad
    attr(edited, "evidence") <- evidence
    expect_null(position_row_unsettled(edited, edited))
    rows <- position_gates(edited, position_policy(0.3, 0.3, 0.15))
    expect_identical(
      gate_status(rows), c(flip = "INDETERMINATE", direction = "INDETERMINATE")
    )
  }
})

count_policy <- function(min_probe_n, flip = 0.15, missing = 0.05) {
  acceptance_policy(
    min_agreement_lower = 0.9, min_n = 50, min_class_n = 15,
    min_recall = 0.8, max_missing_rate = 0,
    max_pairwise_disagreement = NULL,
    required_probes = "position", min_probe_n = min_probe_n,
    max_probe_flip_rate = flip, max_probe_directional_rate = flip,
    max_probe_missing_rate = missing, coefficient = "gwet_ac1"
  )
}

# 100 pairs at repeats = 2; the judge never flips (A, A original; B, B
# swapped) but abstains on one original repeat of items 1-5 (`"abstain"`),
# or answers it B so the order's choices tie (`"tie"`).
count_script <- function(how, k = 5L) {
  function(item, condition) {
    if (condition == "swapped") return(rep("B", 2))
    if (item > k || how == "complete") return(rep("A", 2))
    if (how == "abstain") c("A", NA) else c("A", "B")
  }
}

test_that("the probe count leaves out scored items a completion could leave unscored", {
  policy <- count_policy(min_probe_n = 100)
  complete <- scripted_position(100, count_script("complete"), repeats = 2)
  abstained <- scripted_position(100, count_script("abstain"), repeats = 2)
  tied <- scripted_position(100, count_script("tie"), repeats = 2)
  expect_identical(abstained$n_scored, 100L)
  expect_identical(tied$n_scored, 95L)
  # Positive control: complete, the judge passes.
  rows <- position_gates(complete, policy)
  expect_identical(gate_status(rows), c(flip = "PASS", direction = "PASS"))
  expect_identical(rows$bias_position_n$value, 100)
  expect_identical(rows$bias_position_n$reason, "Sample size is sufficient.")
  # The completion that ties the abstained repeats leaves 95 scored items.
  expect_identical(
    position_gates(tied, policy)$bias_position_n$status, "INDETERMINATE"
  )
  # So abstaining cannot pass on the count those items would lose.
  rows <- position_gates(abstained, policy)
  expect_identical(rows$bias_position_missing_rate$status, "PASS")
  expect_identical(rows$bias_position_n$status, "INDETERMINATE")
  expect_identical(rows$bias_position_n$value, 95)
  expect_identical(
    rows$bias_position_n$reason,
    paste(
      "Sample size is below the declared minimum. The count leaves out the",
      "5 scored items whose missing repeats a completion could answer so",
      "that they are unscored."
    )
  )
  expect_identical(rows$bias_position$status, "INDETERMINATE")
  expect_null(rows$bias_position_direction)
  # With the floor at what every completion keeps, the intervals are
  # assessed and still pass.
  rows <- position_gates(abstained, count_policy(min_probe_n = 95))
  expect_identical(rows$bias_position_n$status, "PASS")
  expect_identical(rows$bias_position_n$value, 95)
  expect_match(
    rows$bias_position_n$reason,
    "^Sample size is sufficient\\. The count leaves out the 5 scored items "
  )
  expect_identical(gate_status(rows), c(flip = "PASS", direction = "PASS"))
})

test_that("a missing repeat that cannot tie its order keeps the item counted", {
  # Repeats = 3, no tie label: A, A, NA keeps A whatever the answer.
  script <- function(item, condition) {
    if (condition == "swapped") return(rep("B", 3))
    if (item <= 5L) c("A", "A", NA) else rep("A", 3)
  }
  bias <- scripted_position(100, script, repeats = 3)
  rows <- position_gates(bias, count_policy(min_probe_n = 100))
  expect_identical(rows$bias_position_n$status, "PASS")
  expect_identical(rows$bias_position_n$value, 100)
  expect_identical(rows$bias_position_n$reason, "Sample size is sufficient.")
  # Without the evidence they may be the ones a completion leaves unscored.
  attr(bias, "evidence") <- NULL
  expect_identical(
    position_gates(bias, count_policy(min_probe_n = 100))$bias_position_n$value, 95
  )
})

test_that("a FAIL needs the probe count every completion keeps", {
  # The always-first judge of the repeats = 2 test above: 15 of 100 pairs miss one
  # original-order repeat, which answered B would tie the item.
  script <- function(item, condition) {
    if (item <= 15L && condition == "original") return(c("A", NA))
    rep("A", 2)
  }
  bias <- scripted_position(100, script, repeats = 2)
  complete <- scripted_position(100, function(item, condition) rep("A", 2),
                                repeats = 2)
  expect_identical(position_row_unsettled(bias, bias)$unscorable, 15L)
  policy <- count_policy(min_probe_n = 90, flip = 0.6, missing = 0.1)
  # Positive control: complete, the biased judge fails at the same floor.
  expect_identical(
    gate_status(position_gates(complete, policy)), c(flip = "FAIL", direction = "FAIL")
  )
  rows <- position_gates(bias, policy)
  expect_identical(rows$bias_position_n$value, 85)
  expect_identical(rows$bias_position$status, "INDETERMINATE")
  rows <- position_gates(bias, count_policy(min_probe_n = 85, flip = 0.6, missing = 0.1))
  expect_identical(gate_status(rows), c(flip = "FAIL", direction = "FAIL"))
})

test_that("without matching evidence the probe count leaves out every scored item that may miss a repeat", {
  abstained <- scripted_position(100, count_script("abstain"), repeats = 2)
  stripped <- abstained
  attr(stripped, "evidence") <- NULL
  expect_null(position_row_unsettled(stripped, stripped))
  rows <- position_gates(stripped, count_policy(min_probe_n = 100))
  expect_identical(rows$bias_position_n$status, "INDETERMINATE")
  expect_identical(rows$bias_position_n$value, 95)
  expect_match(
    rows$bias_position_n$reason,
    paste(
      "The count leaves out the 5 scored items that may have a missing",
      "repeat: the probe's per-judgment evidence that would settle them is",
      "unavailable or does not reproduce this probe's counts."
    ),
    fixed = TRUE
  )
  # At that floor the count passes and the intervals still are not
  # assessed.
  rows <- position_gates(stripped, count_policy(min_probe_n = 95))
  expect_identical(rows$bias_position_n$status, "PASS")
  expect_identical(
    gate_status(rows), c(flip = "INDETERMINATE", direction = "INDETERMINATE")
  )
  # One item reads in the singular, with or without the evidence.
  one <- scripted_position(100, count_script("abstain", k = 1L), repeats = 2)
  expect_match(
    position_gates(one, count_policy(min_probe_n = 100))$bias_position_n$reason,
    "leaves out the 1 scored item whose missing repeats a completion could answer so that it is unscored.",
    fixed = TRUE
  )
  attr(one, "evidence") <- NULL
  expect_match(
    position_gates(one, count_policy(min_probe_n = 100))$bias_position_n$reason,
    "leaves out the 1 scored item that may have a missing repeat: the probe's per-judgment evidence that would settle it is",
    fixed = TRUE
  )
  # A hand-edited count that cannot place them leaves the count unavailable.
  edited <- stripped
  edited$n_items_missing <- NA_integer_
  rows <- position_gates(edited, count_policy(min_probe_n = 50))
  expect_identical(rows$bias_position_n$status, "INDETERMINATE")
  expect_identical(rows$bias_position_n$value, NA_real_)
  expect_match(rows$bias_position$reason, "^Sample size is unavailable;")
  # A hand-edited negative count keeps its own value, as before.
  edited <- stripped
  edited$n_scored <- -5L
  rows <- position_gates(edited, count_policy(min_probe_n = 50))
  expect_identical(rows$bias_position_n$value, -5)
  expect_identical(rows$bias_position_n$reason, "Sample size is below the declared minimum.")
  edited$n_items_missing <- NA_integer_
  expect_identical(
    position_gates(edited, count_policy(min_probe_n = 50))$bias_position_n$value, -5
  )
})
