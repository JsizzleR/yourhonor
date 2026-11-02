# Builds data/synthetic_gold.rda
# Run once from the package root:  source("data-raw/synthetic_gold.R")
#
# A 200-item synthetic calibration set with three human raters, sized so that
# an audit can reach a verdict rather than stop at INDETERMINATE. Every label
# is planted: a true label is drawn per item, the answer text is written to
# match it, and each rater is a noisy reading of the truth. The text is
# templated (24 support questions, one correct and one partial answer each,
# eight deflections), so no row is a real support ticket; what is independent
# across rows, and what the item bootstrap relies on, is the draw of the true
# label and of each rater's error.
#
# The README's judge reads the answer by keyword and carries a planted flaw: an
# input longer than 240 characters is graded one level more generously. Inputs
# here are kept at or under 180 characters, so the originals and the ~56
# character framing prefixes of the self-preference and bandwagon probes stay
# under the threshold while the verbosity probe's 258 character filler crosses
# it. Only the verbosity probe should find the flaw.

library(tibble)

set.seed(20261102)

levels <- c("I", "P", "C")
n <- 200L

questions <- c(
  "How do I reset my password?",
  "Why was my card declined?",
  "Can I export my data to CSV?",
  "What are your support hours?",
  "How do I cancel my subscription?",
  "Is two-factor authentication supported?",
  "Where can I download my invoices?",
  "How do I add a teammate to my account?",
  "How do I change the account email?",
  "Can I get a refund for last month?",
  "Why am I not receiving notification emails?",
  "How do I delete my account?",
  "Does the API have rate limits?",
  "How do I rotate an API key?",
  "Can I change my username?",
  "How do I upgrade my plan?",
  "Where is the audit log?",
  "How do I set up SSO?",
  "Why is my dashboard slow?",
  "Can I share a report outside my team?",
  "How do I restore a deleted project?",
  "What file types can I upload?",
  "How do I change the timezone?",
  "Why does my import fail?"
)

# Correct answers name the place and the action.
correct <- c(
  "Open the sign-in page, click Forgot password, and follow the emailed link.",
  "Open Billing > Payment method; the bank's decline reason is shown under the card.",
  "Open Settings > Export, choose CSV, and click Download.",
  "Use the chat widget 9am to 6pm on weekdays, or email us at any hour.",
  "Open Billing > Plan and click Cancel subscription; it ends at the period's close.",
  "Yes: open Security, click Enable 2FA, and scan the code with an authenticator app.",
  "Open Billing > History and click the PDF icon beside each invoice.",
  "Open Team, click Invite, enter their email, and choose a role.",
  "Open Profile, click Edit beside the email, and confirm from the new address.",
  "Open Billing > History, click the invoice, and use Request refund within 30 days.",
  "Open Notifications, enable Email, then check spam and allow our sending domain.",
  "Open Settings > Account, click Delete account, and confirm with your password.",
  "Yes: 600 requests per minute per key; the limit is returned in X-RateLimit headers.",
  "Open Developer > Keys, click Rotate, and update your app within 24 hours.",
  "Open Profile, click Edit beside the username, and save; old links redirect for 30 days.",
  "Open Billing > Plan, click Upgrade, and pick the plan; the change is prorated today.",
  "Open Admin > Audit log; click Export to download it as CSV.",
  "Open Admin > Security > SSO, enter your IdP metadata URL, and click Test connection.",
  "Open the dashboard's Filters and reduce the date range to 30 days.",
  "Open the report, click Share, and switch on Public link; it expires in 7 days.",
  "Open Projects > Trash and click Restore beside the project within 30 days.",
  "Use PDF, PNG, JPG, CSV or XLSX, each up to 25 MB.",
  "Open Profile > Preferences, choose the timezone, and click Save.",
  "Open Import > Log; the failing row and its reason are listed there."
)

# Partial answers point in the right direction and hedge.
partial <- c(
  "There should be a recovery option somewhere on the sign-in page.",
  "It is probably a bank issue; you could try the card again.",
  "I think there is an export option in Settings somewhere.",
  "Support is around on weekdays, I believe.",
  "You should be able to cancel from somewhere in Billing.",
  "It is supported, probably under one of the security settings.",
  "Invoices should be somewhere in the Billing area.",
  "You can probably invite people from the Team page.",
  "There should be a way to edit it in your profile.",
  "Refunds are sometimes possible; you could ask in a ticket.",
  "It might be a spam filter issue; try checking your settings.",
  "Account deletion is probably somewhere under Settings.",
  "There are some limits, I think they are documented somewhere.",
  "You should be able to make a new key from the developer area.",
  "Usernames can probably be changed in your profile.",
  "Upgrades are handled somewhere in Billing, I believe.",
  "I think admins can find a log of some kind in the admin area.",
  "SSO is available on some plans; it should be under admin settings.",
  "It might be the date range or something; try narrowing things down.",
  "Sharing is probably possible from the report's menu.",
  "Deleted projects can sometimes be recovered, I think.",
  "Most common document and image formats should be fine.",
  "The timezone should be adjustable somewhere in your preferences.",
  "Imports sometimes fail on formatting; you could check the file."
)

# Incorrect answers are deflections: polite, and no help at all.
incorrect <- c(
  "Thanks for reaching out, and sorry for the trouble you are having.",
  "We appreciate your patience while our team looks into this.",
  "That is a great question and one we hear often.",
  "Our product is designed to make your work easier every day.",
  "Please let us know if there is anything else we can help with.",
  "We are always working to improve the experience for all customers.",
  "Your feedback has been noted and passed along to the team.",
  "We hope you are enjoying the platform so far."
)

stopifnot(length(correct) == length(questions), length(partial) == length(questions))

true_label <- sample(levels, n, replace = TRUE, prob = c(0.15, 0.30, 0.55))
q <- sample.int(length(questions), n, replace = TRUE)
answer <- character(n)
answer[true_label == "C"] <- correct[q[true_label == "C"]]
answer[true_label == "P"] <- partial[q[true_label == "P"]]
answer[true_label == "I"] <- sample(incorrect, sum(true_label == "I"), replace = TRUE)

input <- sprintf("Question %d: %s\nAnswer: %s", seq_len(n), questions[q], answer)
stopifnot(all(nchar(input) <= 180L), all(nchar(input) >= 60L))

# Each rater reads the truth with a 6% chance of an adjacent-level slip.
adjacent <- function(label) {
  vapply(label, function(l) switch(l, I = "P", C = "P", P = sample(c("I", "C"), 1L)), "")
}
rate <- function(truth, p_slip = 0.06) {
  slip <- runif(length(truth)) < p_slip
  out <- truth
  out[slip] <- adjacent(truth[slip])
  ordered(out, levels = levels)
}

synthetic_gold <- tibble(
  id = sprintf("syn-%03d", seq_len(n)),
  input = input,
  target = "Does the answer correctly and completely resolve the question?",
  rater_1 = rate(true_label),
  rater_2 = rate(true_label),
  rater_3 = rate(true_label)
)

if (requireNamespace("usethis", quietly = TRUE)) {
  usethis::use_data(synthetic_gold, overwrite = TRUE)
} else {
  dir.create("data", showWarnings = FALSE)
  save(synthetic_gold, file = "data/synthetic_gold.rda", version = 3, compress = "bzip2")
}
