#' @rawNamespace S3method(summary,invarianceCutoffs)
summary.invarianceCutoffs <- function(object, ...) {
  
  cat("Configural Model\n")
  print(object$simulationParameters[[1]])
  cat("\n")
  print(object$summary[[1]])
  
  cat("\nMetric Model\n")
  print(object$simulationParameters[[2]])
  cat("\n")
  print(object$summary[[2]])
  
  cat("\nScalar Model\n")
  print(object$simulationParameters[[3]])
  cat("\n")
  print(object$summary[[3]])
  
  cat("\nStrict Model\n")
  print(object$simulationParameters[[4]])
  cat("\n")
  print(object$summary[[4]])
}