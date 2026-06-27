#' @keywords internal
"_PACKAGE"

#' @importFrom rlang .data
#' @importFrom stats ave binom.test complete.cases p.adjust quantile sd
NULL

# Coefficients understood for the judge-vs-human comparison (two raters);
# `cohen_kappa` is computed via irrCAC's Conger coefficient, which equals Cohen's
# kappa for two raters. The multi-rater human-agreement ceiling uses the subset
# that is defined for more than two raters.
yourhonor_coefficients <- c("cohen_kappa", "krippendorff_alpha", "gwet_ac1")
multirater_coefficients <- c("krippendorff_alpha", "gwet_ac1")

fmt_num <- function(x, digits = 2) {
  formatC(x, format = "f", digits = digits)
}
