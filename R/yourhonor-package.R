#' @keywords internal
"_PACKAGE"

#' @importFrom rlang .data
#' @importFrom stats ave binom.test complete.cases p.adjust quantile sd
NULL

# Coefficients understood for the judge-vs-human comparison (two raters);
# `cohen_kappa` is computed via irrCAC's Conger coefficient, which equals Cohen's
# kappa for two raters. Human reliability uses the subset defined for more than
# two raters; it is a benchmark, not a mathematical ceiling on judge agreement.
yourhonor_coefficients <- c("cohen_kappa", "krippendorff_alpha", "gwet_ac1")
supported_coefficients <- c(yourhonor_coefficients, "gwet_ac2")
multirater_coefficients <- c("krippendorff_alpha", "gwet_ac1")

fmt_num <- function(x, digits = 2) {
  formatC(x, format = "f", digits = digits)
}
