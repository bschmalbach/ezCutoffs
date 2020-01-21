#' Fit Measure Cutoffs in SEM
#' @import ggplot2 lavaan moments progress utils
#' @importFrom doSNOW registerDoSNOW
#' @importFrom foreach %dopar%
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom stats sd median quantile wilcox.test
#' @description Calculate cutoff values for model fit measures used in structural equation modeling (SEM) by simulating and testing data sets (cf. Hu & Bentler, 1999 <doi:10.1080/10705519909540118>) with the same parameters (population model, number of observations, etc.) as the model under consideration.
#' @param model \link[lavaan]{lavaan}-style Syntax of a user-specified model.
#' @param data A data frame containing the variables specified in model.
#' @param n_obs Specifies the number of observations. Only needed if no data frame is given. Can be given as a numeric vector representing the exact group sizes in multigroup analyses. In this case, the grouping variable needs to be called \code{"group"}.
#' @param n_rep Number of replications.
#' @param fit_indices Character vector, containing a selection of fit indices for which to calculate cutoff values. Only measures produced by \link[lavaan]{fitMeasures} can be chosen.
#' @param alpha_level Type I-error rate for the generated cutoff values: Between 0 and 1; 0.05 per default.
#' @param normality Specify distributional assumptions for the simulated data: Either \code{"assumed"} for normal distribution, or \code{"empirical"} for distributions based on the skewness and kurtosis values of the empirical data.
#' @param missing_data Specify handling of missing values: Either \code{FALSE} to generate complete data sets, or \code{TRUE} to generate data with the same number of missing values on the observed variables as in the empirical data.
#' @param bootstrapped_ci Specify whether a boostrapped confidence interval for the empirical model fit statistics should be drawn; default = FALSE.
#' @param n_boot Number of replications in bootstrap for confidence intervalls for empirical model fit statistics.
#' @param boot_alpha Type I-error rate choosen for the boostrap-confidence interval: Between 0 and 1; 0.05 per default.
#' @param boot_internal Whether to use the internal boostrap implemented in \code{bootstrapLavaan} or a standard implementation in the \link{boot} package. Defaults to \code{FALSE}
#' @param n_cores The number of cores to use. If \code{NULL} (the default) all available cores will be used.
#' @param rescale Only relevant, when data are categorial, but assumed as continous. Indicator whether data should be rescaled to have the same mean and variance as the corresponding continous data. Default to \code{FALSE}.
#' @param n_cat Number of categories to categorize simulated continous data. Can be vector or single value to fit all. \code{2} to \code{7} categories are possible for now. If \code{NA}, no categorization is done.
#' @param condition Condition to categorize data by defining the way the category thresholds are drawn (based on Rhemtulla, Brosseau-Liard, & Savalei, 2012). Maybe vector or single value to fit all. All thresholds are drawn from a standard normal distribution. Data are standardized accordingly. Default to \code{"symmetric"} = thresholds are drawn symmetrically, \code{"mod.asym"} = thresholds are drawn moderately asymetrically, \code{"ext.asym"} = thresholds are drawn extremely asymmetrically , \code{"mod.asym-alt"} = alternating moderate asymmetrical condition, \code{"ext.asym-alt"} = alternating extreme asymmetrical condition.
#' @param data_assumption Assumption of how data are measured. Default to \code{"continous"} = data are assumed to be continous. \code{"categorical_assumed_continous"} = data are collected categorical, but are assumed to measure continous variables (e.g., by the use of anchored Likert scales). \code{"categorical"} = data are assumed to be categorical and will be treated as categorical. 
#'
#' @details
#' \code{model} is expected in standard lavaan nomenclature. The typical pre-multiplication mechanism is supported, with the exception of vectors (see Examples). Multigroup models should instead be specified using the \code{group} argument. \cr\cr
#' If \code{data} is not specified, the program will generate data based on the given \code{model} and \code{n_obs}. A numeric vector would signify multiple groups and \code{group} needs to be set to "group" in this case. Otherwise, \code{n_obs} is disregarded. \cr\cr
#' \code{missing_data = TRUE} assumes that the data is missing completely at random. That, is missings should not be distributed unevenly in multigroup models, for instance.\cr\cr
#' \code{bootstrapped_ci = "TRUE"} Returns a nonparametric bootstrap confidence interval that quantifies the uncertainty within a data set with regard to the empirical fit indices. Larger sample sizes should, under ideal circumstances, have smaller confidence intervals. For more information see, e.g., Efron (1981; 1987). Bootstrapping uses the \code{library(boot)} and (if available) several CPUs to compute the confidence intervals via \code{snow}. \cr\cr
#' \code{...} allows the user to pass lavaan arguments to the model fitting procedure. Options include multigroup, repeated measures, growth curve, and multilevel models.
#'
#' @references Efron, D. (1981). Nonparametric estimates of standard error: The jackknife, the bootstrap and other methods, Biometrika,  68(3), 589-599. doi: 10.1093/biomet/68.3.589 \cr
#' @references Efron, B. (1987). Better bootstrap confidence intervals. Journal of the American statistical Association, 82(397), 171-185.
#' @references Hu, L. T., & Bentler, P. M. (1999). Cutoff criteria for fit indexes in covariance structure analysis: Conventional criteria versus new alternatives. Structural Equation Modeling: A Multidisciplinary Journal, 6(1), 1-55. doi: 10.1080/10705519909540118
#' @references Rhemtulla, M., Brosseau-Liard, P. É., & Savalei, V. (2012). When can categorical variables be treated as continuous? A comparison of robust continuous and categorical SEM estimation methods under suboptimal conditions. Psychological Methods, 17, 354–373. doi:10.1037/a0029315
#'
#' @return An object of the class ezCutoffs, containing the function call, arguments, simulation statistics, the simulated and (if given) the empirical data sets, fit distributions, the empirical model, and a summary.
#' For ease of analysis, it is further inspectable via \code{print}, \code{summary}, \code{plot}, 
#' \code{\link{compareFit}}, and \code{\link{invarianceCutoffs}}.
#'
#' @examples
#' ## model specification examples
#'
#' # simple uni-factorial model
#' model1 <- "F1 =~ a1 + a2 + a3 + a4 + a5"
#'
#' # path model
#' model2 <- "m ~ 0.6*x1
#'            m ~ 0.5*x2
#' 	   m ~ 0.4*x3
#' 	   y ~ 0.7*m"
#'
#' # two-factorial model with some exemplary pre-multiplications
#' model3 <- "F1 =~ NA*a1 + a2 + a3 + 0.8*a4 + a5
#'            F2 =~ b1 + start(0.8)*b2 + b3 + equal('F2 =~ b2')*b4 + b5
#'            F1 ~~ 0*F2"
#'
#' ## function call
#' out <- ezCutoffs(model = model1, n_obs = 1000, n_rep = 10, n_cores = 1)
#' \donttest{
#' out <- ezCutoffs(
#'   model = model1, n_obs = c(300, 400), n_rep = 9999, fit_indices = c("cfi.robust"),
#'   estimator = "MLM", group = "group", group.equal = c("loadings", "intercepts"), n_cores = 1
#' )
#' }
#'
#' ## retrieve output
#' summary(out)
#' plot(out)
#' @seealso \code{\link{compareFit}}

#' @export
ezCutoffs <- function(model = NULL,
                      data = NULL,
                      n_obs = NULL,
                      n_rep = 1000,
                      fit_indices = c("chisq", "cfi", "tli", "rmsea", "srmr"),
                      alpha_level = 0.05,
                      normality = "assumed",
                      missing_data = FALSE,
                      bootstrapped_ci = FALSE,
                      n_boot = 1000,
                      boot_alpha = 0.05,
                      boot_internal = FALSE,
                      n_cores = NULL,
                      rescale = F,
                      n_cat = 5,
                      condition = "symmetric",
                      data_assumption = "continous",
                      ...) {

  #------------------------------------------------------------------------------
  
  # Occurences of c( in model
  if (grepl("\\c\\(", model) == T) {
    stop('Pre-multiplication with vectors is not supported. Please use "group.equal = ..." instead.')
  }
  
  # Create data if none is given
  no_emp_data <- F
  if (is.null(data)) {
    no_emp_data <- T
    data <- simulateData(model, sample.nobs = n_obs)
    if (length(n_obs) > 1) {
      if (!('group'%in%names(list(...)))) {
        stop('Please provide a name for the grouping variable via "group = ...".')
      } else {
        names(data)[names(data) == 'group'] <- list(...)$group
      }
    }
  }
  
  #fit empirical
  arg <- names(formals(empiricalFit))
  arg <- arg[-length(arg)]
  emp <- do.call('empiricalFit', c(mget(arg), list(...)))
  for (i in seq_along(emp)) assign(names(emp)[i], emp[[i]])
  
  # checking data properties: continous, assumed continous or categorical ---------
  if(!(data_assumption %in% c("continous", "categorical_assumed_continous", "categorical"))) stop("data_assumption is not correctly specified. See documentation for help!")
  if(data_assumption == "categorical_assumed_continous")
  {
       if(any(n_cat[!is.na(n_cat)] > 7)) stop("Only less than 8 categories are possible for now... Use NA instead for treating variables as continous!")
       if( (length(condition) > 1 & dim(vartable(fit))[1] != length(condition)) |
           (length(n_cat) > 1 & length(n_cat) != dim(vartable(fit))[1])) stop("Wrong dimension of conditions or number of categories for the categorization of continous data.")
       
       if(length(emp$groups_var) != 0) # checking, whether grouping is done in analysis
       {
            if(length(n_cat) == 1){ n_cat <- c(rep(n_cat, (dim(vartable(fit))[1])), NA)}else{ n_cat <- c(n_cat, NA)}
            if(length(condition) == 1){ condition <- c(rep(condition, (dim(vartable(fit))[1])), NA) }else{ condition <- c(condition, NA)}
            # NA, as the grouping variable should not be categorized or rescaled!
       }
  }
  

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

  # sanity check: fit_indices in empirical fit?-----------------------------------
  length_di <- length(fit_indices)
  for (i in 1:length_di) {
    name_warn <- fit_indices[i]
    if (fit_indices[i] %in% names(empirical_fit) == F) {
      stop(c("The following fit measure is not available with the given parameters: ", name_warn))
    }
  }

  # generate random data----------------------------------------------------------
  arg <- names(formals(dataGeneration))
  data_s_list <- do.call('dataGeneration', c(mget(arg)))
  
  
  # change scaling of data if requested: from continous to assumed continous (categorical) -----
  if(data_assumption == "categorical_assumed_continous")
  {
       message("Data are being categorized, but assumed to be continous.")
       datalist <- data_s_list
       arg <- names(formals(categorize_data_list))
       data_s_list <- do.call('categorize_data_list', c(mget(arg)))
       rm(datalist) # remove continous data --> could be saved for later use also
  }

  
  

  # add missings if requested
  if (missing_data == T) {
    data_s_list <- lapply(data_s_list, missingData, fit, data, n_rep, missing_data, dots)  
  }

  # fit simulated data------------------------------------------------------------
  arg <- names(formals(simFit))
  arg <- arg[-length(arg)]
  sim <- do.call('simFit', c(mget(arg), list(...)))
  for (i in seq_along(sim)) assign(names(sim)[i], sim[[i]])
  
  # calculate descriptives--------------------------------------------------------
  arg <- names(formals(calcDesc))
  fit_simresults <- do.call('calcDesc', c(mget(arg)))

  # simulation stats--------------------------------------------------------------
  arg <- names(formals(simulationStats))
  simulation_stats <- do.call('simulationStats', c(mget(arg)))
 
  # include bootstrapping CI in ouput --------------------------------------------
  if (bootstrapped_ci == T) {
    fit_simresults <- cbind(fit_simresults, boot_ci)
  }
  
  # generate output---------------------------------------------------------------
  call <- as.list(match.call())
  call <- call[-1]
  call[["model"]] <- model
  call[["dots"]] <- dots
  
  if (exists("n_rep")==F) {n_rep <- 1000}
  if (exists("fit_indices")==F) {fit_indices <- c("chisq", "cfi", "tli", "rmsea", "srmr")}
  if (exists("alpha_level")==F) {alpha_level <- .05}
  if (exists("bootstrapped_ci")==F) {bootstrapped_ci <- F}
  if (exists("n_boot")==F) {n_boot <- 1000}
  if (exists("boot_alpha")==F) {boot_alpha <- .05}
  if (exists("boot_internal")==F) {boot_internal <- F}
  
  arguments <- list("n_rep"=n_rep, "fit_indices"=fit_indices, "alpha_level"=alpha_level, "bootstrapped_ci"=bootstrapped_ci, 
                    "n_boot"=n_boot, "boot_alpha"=boot_alpha, "boot_internal"=boot_internal, "no_emp_data"=no_emp_data)
  ezCutoffs_out <- list("call" = call, "simulationParameters" = simulation_stats, "data" = data, "simData" = data_s_list, "fitDistributions" = fit_distributions, "summary" = fit_simresults, "empiricalModel" = emp$fit, "arguments" = arguments)
  class(ezCutoffs_out) <- "ezCutoffs"
  
  return(ezCutoffs_out)
}
