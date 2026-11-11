#' Error classes
#'
#' Every error yourhonor raises about what it was given carries one family
#' class and the umbrella class `yourhonor_error`, so a caller can catch a
#' family with [tryCatch()] or [rlang::try_fetch()] without matching message
#' text. Message wording is for people and may change; the classes are the
#' contract.
#'
#' * `yourhonor_input_error`: an argument or data set the caller passed is
#'   malformed, inconsistent, or missing something required (a column, a
#'   label universe, a unique identifier, a whole-number count).
#' * `yourhonor_judge_error`: the caller's judge function broke its output
#'   contract (wrong shape or length, keys it was not issued, a label outside
#'   the declared universe).
#' * `yourhonor_policy_error`: [acceptance_policy()] was given an invalid
#'   threshold, or a policy cannot be applied to the calibration it is
#'   assessed against (it names a coefficient the calibration did not
#'   compute, or is not a policy at all).
#' * `yourhonor_binding_error`: secondary evidence cannot be bound to the
#'   calibration it is reported with: the `audit_id` values differ, or an
#'   artifact that requires binding was asked to carry unbound evidence.
#'
#' Errors that signal a bug in yourhonor itself ("Internal error: ...") carry
#' no family class. Errors raised by rlang's own argument helpers (for
#' example an unknown choice for `output` or `format`) carry rlang's classes,
#' except in [acceptance_policy()], which re-signals them as
#' `yourhonor_policy_error` with the original as `parent`.
#'
#' @name yourhonor-errors
#' @aliases yourhonor_error yourhonor_input_error yourhonor_judge_error
#'   yourhonor_policy_error yourhonor_binding_error
#' @keywords internal
#' @examples
#' res <- tryCatch(
#'   agreement(data.frame(a = "x", b = "y"), rater_cols = c("a", "b")),
#'   yourhonor_input_error = function(err) "bad input"
#' )
#' res
NULL

input_error <- "yourhonor_input_error"
judge_error <- "yourhonor_judge_error"
# Condition class shared by every error acceptance_policy() raises.
policy_error <- "yourhonor_policy_error"
binding_error <- "yourhonor_binding_error"

# cli_abort() with a family class and the umbrella class. The defaults mirror
# cli_abort()'s own, one frame up: the message is interpolated in, and the
# error reported as coming from, the function that called yh_abort().
yh_abort <- function(message,
                     ...,
                     class = input_error,
                     call = .envir,
                     .envir = parent.frame()) {
  cli::cli_abort(
    message, ...,
    class = c(class, "yourhonor_error"),
    call = call, .envir = .envir, .frame = .envir
  )
}
