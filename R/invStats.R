invStats <- function(fit_distributions, n_rep, fit_s_list) {
  
  n_conv <- sum(!is.na(fit_distributions[, 1]))
  
  inv_stats <- data.frame(matrix(c(n_rep, n_conv), 1, 2))
  names(inv_stats) <- c("#Runs", "#Converged")
  rownames(inv_stats) <- ""

  return(inv_stats)
}
