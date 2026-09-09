#' Plot a judge-versus-human confusion matrix
#'
#' A quick ggplot2 heatmap of where the judge and humans diverge. Requires the
#' Suggested ggplot2 package. Axes follow the resolved `label_levels` order:
#' judge labels left to right and human labels top to bottom, so the agreement
#' diagonal runs from the top-left corner to the bottom-right.
#'
#' @param x An `yourhonor_calibration` object or a ratings data frame.
#' @param normalize Passed to [confusion()]; defaults to `"row"` so each row
#'   shows how a human label was graded by the judge.
#' @param ... Further arguments passed to [confusion()], such as
#'   `label_levels`, `human_col`, and `judge_col`.
#'
#' @return A ggplot object.
#' @export
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   ratings <- data.frame(
#'     human = c("C", "C", "P", "I", "C", "P", "I", "C"),
#'     judge = c("C", "C", "C", "I", "C", "P", "P", "C")
#'   )
#'   plot_confusion(ratings, label_levels = c("I", "P", "C"))
#' }
plot_confusion <- function(x, normalize = "row", ...) {
  rlang::check_installed("ggplot2", reason = "to plot a confusion matrix.")
  conf <- confusion(x, normalize = normalize, ...)
  # ggplot2 sorts discrete axes alphabetically unless the columns are factors,
  # which would silently drop the declared (often ordinal) label order.
  label_levels <- attr(conf, "label_levels", exact = TRUE)
  conf$judge_label <- factor(conf$judge_label, levels = label_levels)
  conf$human_label <- factor(conf$human_label, levels = rev(label_levels))
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
