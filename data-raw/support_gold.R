# Builds data/support_gold.rda
# Run once from the package root:  source("data-raw/support_gold.R")

library(tibble)

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
partial_answers <- c(
  "Go to the sign-in page and look for account recovery options.",
  "Try the card again or contact support.",
  "An export option is available in Settings.",
  "Support is available on weekdays.",
  "You can manage the subscription from Billing.",
  "Two-factor authentication is available in your account settings.",
  "Invoices are available somewhere in Billing.",
  "Use the Team page to manage teammates."
)

idx <- seq_along(questions)
# Keep one row per synthetic question. Repeating a small set of templates would
# create pseudoreplication and make item-bootstrap intervals look too narrow.
human_label <- c("P", "C", "I", "C", "P", "C", "I", "C")
wrong_idx <- (idx %% length(questions)) + 1L
candidate_answer <- ifelse(
  human_label == "C", answers[idx],
  ifelse(human_label == "P", partial_answers[idx], answers[wrong_idx])
)
input <- sprintf(
  "Question: %s\nAnswer: %s",
  questions[idx], candidate_answer
)
target <- "Does the answer correctly and completely resolve the question?"

# The example judge is slightly lenient on two items.
judge_label <- human_label
judge_label[c(1, 7)] <- c("C", "P")

support_gold <- tibble(
  id = seq_along(questions),
  input = input,
  target = target,
  human_label = ordered(human_label, levels = levels),
  judge_label = ordered(judge_label, levels = levels)
)

if (requireNamespace("usethis", quietly = TRUE)) {
  usethis::use_data(support_gold, overwrite = TRUE)
} else {
  dir.create("data", showWarnings = FALSE)
  save(support_gold, file = "data/support_gold.rda", version = 3, compress = "bzip2")
}
