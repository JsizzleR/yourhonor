# Builds data/support_gold.rda
# Run once from the package root:  source("data-raw/support_gold.R")

library(tibble)

set.seed(42)
n <- 150
levels <- c("I", "P", "C")

questions <- c(
  "How do I reset my password?",
  "Why was my card declined?",
  "Can I export my data to CSV?",
  "What are your support hours?",
  "How do I cancel my subscription?",
  "Is two-factor authentication supported?",
  "Where can I download my invoices?",
  "How do I add a teammate to my account?"
)
answers <- c(
  "Use the 'Forgot password' link on the sign-in page.",
  "Your bank declined the charge; please contact them.",
  "Yes — open Settings, then Export, and choose CSV.",
  "We are available 9am to 6pm on weekdays.",
  "Open Billing and click Cancel subscription.",
  "Yes, enable it under Security settings.",
  "Invoices live under Billing > History.",
  "Invite them from the Team page with their email."
)

idx <- sample.int(length(questions), n, replace = TRUE)
input <- sprintf(
  "Question: %s\nAnswer: %s",
  questions[idx], answers[idx]
)
target <- "Does the answer correctly and completely resolve the question?"

# Human labels, skewed toward Correct (realistic for a tuned support bot).
human_label <- sample(levels, n, replace = TRUE, prob = c(0.15, 0.25, 0.60))

# Judge mostly agrees, but is a touch lenient: it sometimes promotes Partial to
# Correct, which is exactly the kind of skew Gwet's AC1 handles better than kappa.
judge_label <- unname(vapply(human_label, function(h) {
  r <- runif(1)
  if (h == "P" && r < 0.30) {
    "C"
  } else if (h == "I" && r < 0.12) {
    "P"
  } else if (r < 0.05) {
    sample(levels, 1)
  } else {
    h
  }
}, character(1)))

support_gold <- tibble(
  id = seq_len(n),
  input = input,
  target = target,
  human_label = factor(human_label, levels = levels),
  judge_label = factor(judge_label, levels = levels)
)

if (requireNamespace("usethis", quietly = TRUE)) {
  usethis::use_data(support_gold, overwrite = TRUE)
} else {
  dir.create("data", showWarnings = FALSE)
  save(support_gold, file = "data/support_gold.rda", version = 3, compress = "bzip2")
}

