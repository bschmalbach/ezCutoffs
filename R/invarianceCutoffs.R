#' Fit Measure Cutoffs for Invariance Testing in Multigroup SEM
#' @description A convenience function for determining cutoff values in multigroup invariance testing. Increasingly constrained models (cf. Meredith, 1993 <doi:10.1007/BF02294825>) are specified and compared in randomly generated data sets. Only the result of \code{\link{ezCutoffs}} is required as input.
#' @param input An object of the class \code{ezCutoffs}. Result of an \code{ezCutoffs} analysis of the configural model.
#' @param ...	Additional arguments to pass to \link[lavaan]{lavaan}. Ellipsis arguments already present in the call to \code{ezCutoffs} ought not to be repeated, since this can cause conflicts.
#'
#' @details
#' \code{group} needs to be specified during \code{ezCutoffs()} analysis.\cr\cr
#' \code{group.equal} should not be specified because the function automatically tests configural, metric, scalar, and strict models, \code{group.partial} is valid. \cr\cr
#' Most other arguments should be specified in \code{ezCutoffs()} to yield accurate comparisons.
#' 
#' @references Meredith, W. (1993). Measurement invariance, factor analysis and factorial invariance. Psychometrika, 58(4), 525-543. doi: https://doi.org/10.1007/BF02294825
#'
#' @return An object of the class invarianceCutoffs, containing function calls, simulation parameters, fit distributions, the empirical model, and a summary.
#' For ease of analysis, it is further inspectable via \code{print}, \code{summary}, \code{plot}, and \code{\link{compareFit}}
#'
#' @examples
#' ## model specification examples
#'
#' # simple uni-factorial model
#' model1 <- "F1 =~ a1 + a2 + a3 + a4 + a5"
#'
#' out <- ezCutoffs(model1, n_obs = c(200, 200, 200), n_rep = 10, group = "group", n_cores = 1)
#' 
#' ## function call
#' out <- invarianceCutoffs(input = out)
#'
#' ## retrieve output
#' print(out)
#' summary(out)
#' 
#' @seealso \code{\link{ezCutoffs}}

#' @rawNamespace importFrom("methods", "is")

#' @export
invarianceCutoffs <- function(input = NULL,
                              ...) {
  
  #------------------------------------------------------------------------------
  
  # check input file
  if (is(input, "ezCutoffs")==F) {stop("input is not of the class 'ezCutoffs'")}
  
  # unpack input
  for (i in seq_along(input)) assign(names(input)[i], input[[i]])
  for (i in seq_along(call)) assign(names(call)[i], call[[i]])
  for (i in seq_along(arguments)) assign(names(arguments)[i], arguments[[i]])
  data_s_list <- simData
  
  #compare dots, o_dotsdots and arguments from call
  o_dots <- dots
  dots <- list(...)
          
  if (sum(dots %in% o_dots)>0) {
    warning(paste("Overlapping arguments in ... from invarianceCutoffs() and additional arguments passed from ezCutoffs(): ", 
                  dots[names(dots) %in% names(o_dots)],". This could lead to incorrect results."))
  }
  if (sum(dots %in% arguments)>0) {
    warning(paste("Overlapping arguments in ... from invarianceCutoffs() and arguments passed from ezCutoffs(): ", 
                  dots[names(dots) %in% names(arguments)],". This could lead to incorrect results."))
    warning(dots[names(dots) %in% names(arguments)])
  }
  
  # checks
  if (exists("group")==F) {"No grouping variable has been defined."}
  if (exists("n_cores")==F) {n_cores <- NULL}
  
  # justFit
  arg <- names(formals(justFit))
  arg <- arg[-length(arg)]
  message("\nMetric invariance")
  invFit_metric <- do.call('justFit', c(mget(arg), group.equal="loadings", o_dots, list(...)))
  message("\nScalar invariance")
  invFit_scalar <- do.call('justFit', c(mget(arg), list(group.equal=c("loadings", "intercepts")), o_dots, list(...)))
  message("\nStrict invariance")
  invFit_strict <- do.call('justFit', c(mget(arg), list(group.equal=c("loadings", "intercepts", "residuals")), o_dots, list(...)))
  
  # unpack and make tables
  
  old_call <- call
  new_call <- as.list(match.call())
  calls <- list(old_call, new_call)
  
  simPar<-list("conf_simPar" = simulationParameters)
  fitDist<-list("conf_fitDist" = fitDistributions)
  simSum<-list("conf_Summary" = summary)
  empMod<-list("conf_EmpMod" = empiricalModel)
  
  for (i in seq_along(invFit_metric)) assign(names(invFit_metric)[i], invFit_metric[[i]])
  simPar[["metric"]] <- simulationParameters
  fitDist[["metric"]] <- fitDistributions
  simSum[["metric"]] <- summary
  empMod[["metric"]] <- empiricalModel
  
  for (i in seq_along(invFit_scalar)) assign(names(invFit_scalar)[i], invFit_scalar[[i]])
  simPar[["scalar"]] <- simulationParameters
  fitDist[["scalar"]] <- fitDistributions
  simSum[["scalar"]] <- summary
  empMod[["scalar"]] <- empiricalModel
  
  for (i in seq_along(invFit_strict)) assign(names(invFit_strict)[i], invFit_strict[[i]])
  simPar[["strict"]] <- simulationParameters
  fitDist[["strict"]] <- fitDistributions
  simSum[["strict"]] <- summary
  empMod[["strict"]] <- empiricalModel
  
  
  
  # deltas
  deltas <- simSum[[1]][,1:3]
  names(deltas)<-c("metric", "scalar", "strict")
  
  high_cut_index <- c(
    "chisq", "chisq.scaled", "fmin", "aic", "bic", "bic2", "rmsea", "rmsea.scaled", "rmsea.ci.upper.scaled", "rmsea.robust",
    "rmsea.ci.upper.robust", "rmsea.ci.upper", "rmr", "rmr_nomean", "srmr", "srmr_bentler", "srmr_bentler_nomean", "crmr",
    "crmr_nomean", "srmr_mplus", "srmr_mplus_nomean", "ecvi"
  )
  
  low_cut_index <- c(
    "pvalue", "pvalue.scaled", "cfi", "tli", "nnfi", "rfi", "nfi", "pnfi", "ifi", "rni", "cfi.scaled", "tli.scaled", "cfi.robust", "tli.robust",
    "nnfi.scaled", "nnfi.robust", "rfi.scaled", "nfi.scaled", "ifi.scaled", "rni.scaled", "rni.robust", "logl", "unrestricted.logl", "gfi",
    "agfi", "pgfi", "mfi", "rmsea.pvalue", "rmsea.pvalue.scaled", "rmsea.pvalue.robust", "cn_05", "cn_01"
  )
  
  for (i in 1:nrow(deltas)) {
    if ((fit_indices[i] %in% high_cut_index) == T) {
      deltas[i,1] <- stats::quantile((fitDist[["metric"]][,i] - fitDist[["conf_fitDist"]][,i]), probs = (1-alpha_level), na.rm = T)
      deltas[i,2] <- stats::quantile((fitDist[["scalar"]][,i] - fitDist[["metric"]][,i]), probs = (1-alpha_level), na.rm = T)
      deltas[i,3] <- stats::quantile((fitDist[["strict"]][,i] - fitDist[["scalar"]][,i]), probs = (1-alpha_level), na.rm = T)
    } else if ((fit_indices[i] %in% low_cut_index) == T) {
      deltas[i,1] <- stats::quantile((fitDist[["metric"]][,i] - fitDist[["conf_fitDist"]][,i]), probs = (1-alpha_level), na.rm = T)
      deltas[i,2] <- stats::quantile((fitDist[["scalar"]][,i] - fitDist[["metric"]][,i]), probs = (1-alpha_level), na.rm = T)
      deltas[i,3] <- stats::quantile((fitDist[["strict"]][,i] - fitDist[["scalar"]][,i]), probs = (1-alpha_level), na.rm = T)
    } else {
      deltas[i,1:3] <- NA
    }
  }
  
  
  # generate output---------------------------------------------------------------
  out <- list("calls"=calls, "simulationParameters" = simPar, "fitDistributions" = fitDist, "summary" = simSum, "empiricalModel" = empMod, "deltas" = deltas)
  class(out) <- "invarianceCutoffs"
  
  return(out)
}
