#' @export
print.invarianceCutoffs <- function(x, ...) {
  filter <- c(grep("Empirical", names(x$summary[[1]])), grep("Cutoff", names(x$summary[[1]])))
  
  cat("Configural Model\n")
  conf_t <- x$summary[[1]][, filter]
  print(conf_t)
  
  cat("\nMetric Model\n")
  metric_t <- x$summary[[2]][, filter]
  metric_t[,3] <- x$deltas[[1]]
  names(metric_t)[3] <- "Delta cutoff"
  print(metric_t)

  cat("\nScalar Model\n")
  scalar_t <- x$summary[[3]][, filter]
  scalar_t[,3] <- x$deltas[[2]]
  names(scalar_t)[3] <- "Delta cutoff"
  print(scalar_t)

  cat("\nStrict Model\n")
  strict_t <- x$summary[[4]][, filter]
  strict_t[,3] <- x$deltas[[3]]
  names(strict_t)[3] <- "Delta cutoff"
  print(strict_t)
}
