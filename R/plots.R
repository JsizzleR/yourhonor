#' Plot a judge-versus-human confusion matrix
#'
#' A quick ggplot2 heatmap of where the judge and humans diverge. Requires the
#' Suggested ggplot2 package.
#'
#' @param x An `yourhonor_calibration` object or a two-column ratings data frame.
#' @param normalize Passed to [confusion()]; defaults to `"row"` so each row
#'   shows how a human label was graded by the judge.
#'
#' @return A ggplot object.
#' @export
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   ratings <- data.frame(
#'     human = c("C", "C", "P", "I", "C", "P", "I", "C"),
#'     judge = c("C", "C", "C", "I", "C", "P", "P", "C")
#'   )
#'   plot_confusion(ratings)
#' }
plot_confusion <- function(x, normalize = "row") {
  rlang::check_installed("ggplot2", reason = "to plot a confusion matrix.")
  conf <- confusion(x, normalize = normalize)
  ggplot2::ggplot(
    conf,
    ggplot2::aes(x = .data$judge_label, y = .data$human_label, fill = .data$prop)
  ) +
    ggplot2::geom_tile(color = "gray90") +
    ggplot2::geom_text(ggplot2::aes(label = .data$n)) +
    ggplot2::scale_fill_gradient(low = "white", high = "steelblue", limits = c(0, 1)) +
    ggplot2::labs(x = "Judge label", y = "Human label", fill = "Proportion") +
    ggplot2::theme_minimal()
}
