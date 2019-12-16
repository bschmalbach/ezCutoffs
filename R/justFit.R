justFit <- function(model, data, data_s_list, n_rep, fit_indices, alpha_level,
                    bootstrapped_ci, n_boot, boot_alpha, boot_internal, 
                    n_cores, no_emp_data, ...) {
  
  #------------------------------------------------------------------------------
  
  #fit empirical
  arg <- names(formals(empiricalFit))
  arg <- arg[-length(arg)]
  emp <- do.call('empiricalFit', c(mget(arg), list(...)))
  for (i in seq_along(emp)) assign(names(emp)[i], emp[[i]])
  
  # parallel processing setup ----------------------------------------------------
  if (is.null(n_cores)) {
    n_cores <- parallel::detectCores()
  } else if (!is.numeric(n_cores)) {
    n_cores <- 1
  }
  
  # empirical fit - bootstrapped CI ----------------------------------------------
  if (bootstrapped_ci) {
    arg <- names(formals(bootstrap))
    arg <- arg[-length(arg)]
    boot_ci <- do.call('bootstrap', c(mget(arg), list(...)))
  }
  
  # fit simulated data------------------------------------------------------------
  arg <- names(formals(simFit))
  arg <- arg[-length(arg)]
  sim <- do.call('simFit', c(mget(arg), list(...)))
  for (i in seq_along(sim)) assign(names(sim)[i], sim[[i]])
  length_di <- length(fit_indices)
  
  # calculate descriptives--------------------------------------------------------
  arg <- names(formals(calcDesc))
  fit_simresults <- do.call('calcDesc', c(mget(arg)))
  
  # simulation stats--------------------------------------------------------------
  arg <- names(formals(invStats))
  inv_stats <- do.call('invStats', c(mget(arg)))
  
  # include bootstrapping CI in ouput --------------------------------------------
  if (bootstrapped_ci == T) {
    fit_simresults <- cbind(fit_simresults, boot_ci)
  }
  
  # generate output---------------------------------------------------------------
  justFit_out <- list("simulationParameters" = inv_stats, "fitDistributions" = fit_distributions, "summary" = fit_simresults, "empiricalModel" = emp$fit)
  class(justFit_out) <- "justFit"
  
  return(justFit_out)
}