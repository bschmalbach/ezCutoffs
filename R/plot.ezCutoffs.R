if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("count"))
}

#' @export
plot.ezCutoffs <- function(x, ...) {
  for (i in 1:nrow(x[["summary"]])) {
    hist_plots <- ggplot2::ggplot(x[["fitDistributions"]], ggplot2::aes_string(x = rownames(x[["summary"]])[i])) +
      ggplot2::geom_histogram(ggplot2::aes(y = stat(count)), color = "black", fill = "white", bins = 30)
    
    bar_height <- max(ggplot2::ggplot_build(hist_plots)$data[[1]]$count)
    
    hist_plots <- hist_plots + ggplot2::geom_vline(xintercept = x[["summary"]][i, 1], color = "blue", alpha = 0.8) +
      ggplot2::annotate("text", x = x[["summary"]][i, 1], y = bar_height, angle = 0, label = "Empirical") +
      ggplot2::geom_vline(xintercept = x[["summary"]][i, 5], color = "red", alpha = 0.8) +
      ggplot2::annotate("text", x = x[["summary"]][i, 5], y = bar_height, angle = 0, label = "Cutoff") +
      ggplot2::labs(title = "Simulated Fit Distribution", y = "count") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    (print(hist_plots))
  }
}