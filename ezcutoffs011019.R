#lavaan model syntax

model1 <- "F1 =~ a1 + a2 + a3 + a4 + a5"

model2 <- "F1 =~ NA*a1 + a2 + a3 + 0.8*a4 + a5
          F2 =~ b1 + start(0.8)*b2 + b3 + equal('F2 =~ b2')*b4 + b5
          F1 ~~ 0*F2"

#generate data
data1 <- simulateData(model1, sample.nobs = 1000)
data2 <- simulateData(model2, sample.nobs = 1000)

#for multigroup cfa
#data[,(ncol(data)+1)] <- sample(c(1,2,3),replace = T, size=1000)
#names(data)[ncol(data)] <- "s2"
#names(data)



#dependencies------------------------------------------------------------------

library(lavaan)
library(ggplot2)
library(pbapply)


#-------------------------------------------------------------------------------

ezCutoffs <- function(model = NULL,
                      data = NULL,
                      n_rep = 1000,
                      which_fit = c("chisq", "cfi", "tli", "rmsea", "srmr"),
                      alpha_level = 0.05,
                      n_obs = NULL,   # se="none"
                      ...) {

  #empirical fit-----------------------------------------------------------------


 # #Occurences of c( in model
 if (grepl("\\c\\(", model) == T) {stop("Pre-multiplication with vectors is not supported.")}

  #Create data if none is given
  if (is.null(data)) {
    data <- simulateData(model, sample.nobs = n_obs)
  }

  fit <- sem(model = model, data = data, ...)

  fit_measures <- fitMeasures(fit)
  #suppressWarnings(more_fit <- moreFitIndices(fit))

  empirical_fit <- fit_measures
  #empirical_fit <- c(fit_measures, more_fit)

  #sanity check: which_fit in empirical fit?--------------------------------------
  length_di <- length(which_fit)
  for (i in 1:length_di) {
    name_warn <- which_fit[i]
    if (which_fit[i] %in% names(empirical_fit) == F) 
      {stop(c("The following fit measure is not available with the given parameters: ", name_warn))}
  }

  #input parameters-------------------------------------------------------------
  pop_model <- parTable(fit)
  n <- vartable(fit)[1,"nobs"]
  groups_var <- lavInspect(fit, what="group")
  n_groups <- lavInspect(fit, what="ngroups")
  group_labels <- lavInspect(fit, what="group.label")
  #stop if grouping variable wiht only one level has been selected
  if ((length(groups_var)>0) & n_groups < 2) {stop("Less than 2 levels in the defined grouping variable.")}

  #generate random data----------------------------------------------------------

  cat("\nData Generation\n")

  if (n_groups > 1) {
    group_sizes <- as.data.frame(table(get(groups_var, data)))[,2]
    data_generation <- function() {
      simulateData(pop_model, sample.nobs = group_sizes, group.label = group_labels)
    }
    data_s_list <- vector("list", n_rep)
    data_s_list <- pbreplicate(n_rep, data_generation(), simplify = F)

      ###### CHANGE NAME GROUP TO groups_var
    for (i in 1:n_rep) {
      old_names <- names(data_s_list[[i]])
      new_names <- gsub("group", get("groups_var"), old_names)
      names(data_s_list[[i]]) <- new_names
    }

  } else {
    data_generation <- function() {
      simulateData(pop_model, sample.nobs = n)
    }
    data_s_list <- vector("list", n_rep)
    data_s_list <- pbreplicate(n_rep, data_generation(), simplify = F)
  }

  #fit data in lavaan for fitmeasures()----------------------------------------------
  cat("\nModel Fitting\n")

  pb <- timerProgressBar(max = n_rep, char = "+", width = 50)

  fit_s_list <- vector("list", length = n_rep)
  for (i in 1:n_rep) {
    fit_s <- sem(model = model, data = data_s_list[[i]], ...)
    fit_s_list[[i]]<-fit_s
    setTimerProgressBar(pb, i)
  }

  closepb(pb)

  #extract fit_measures-----------------------------------------------------------
  fit_measures_s_list <- list()
  for (i in 1:n_rep) {
    fit_measures_s <- fitmeasures(fit_s_list[[i]], fit.measures = which_fit)
    fit_measures_s_list[[i]] <- fit_measures_s
  }

  #more_fit_s_list <- list()
  #for (i in 1:n_rep) {
  #  suppressWarnings(more_fit_s <- moreFitIndices(fit_s_list[[i]]), fit.measures = which_fit)
  #  more_fit_s_list[[i]]<-more_fit_s
  #}


  #get fit distributions------------------------------------------------------------

  m_fit_measures <- matrix(NA,n_rep,length(fit_measures_s_list[[1]]))
  #m_more_fit <- matrix(NA,n_rep,length(more_fit_s_list[[1]]))

  for (i in 1:n_rep) {
    m_fit_measures[i,] <- matrix(unlist(fit_measures_s_list[[i]]), ncol = length(fit_measures_s_list[[i]]))
    #m_more_fit[i,] <- matrix(unlist(more_fit_s_list[[i]]), ncol = length(more_fit_s_list[[i]]))
  }

  fit_distributions <- as.data.frame(m_fit_measures)
  #fit_distributions <- cbind(m_fit_measures, m_more_fit)

  ncols <- length(fit_measures_s_list[[1]])
  #ncols <- length(fit_measures_s_list[[1]]) + length(more_fit_s_list[[1]])
  names(fit_distributions)[1:length(fit_measures_s_list[[1]])] <- names(fit_measures_s_list[[1]])
  #names(fit_distributions)[(length(fit_measures_s_list[[1]])+1):ncols] <- names(more_fit_s_list[[1]])


  #calculate descriptives----------------------------------------------------
  fit_simresults <- data.frame(matrix(NA,length_di,5))
  percentage_a <- 100*alpha_level
  cutoff_name <- paste(percentage_a, "% Cutoff", collapse = "", sep = "")
  names(fit_simresults) <- c("Empirical fit", "Simulation Mean", "Simulation SD", "Simulation Median", cutoff_name)
  rownames(fit_simresults) <- which_fit

  high_cut_index <- c("chisq", "chisq.scaled", "fmin", "aic", "bic", "bic2", "rmsea", "rmsea.scaled", "rmsea.ci.upper.scaled", "rmsea.robust",
                      "rmsea.ci.upper.robust", "rmsea.ci.upper", "rmr", "rmr_nomean", "srmr", "srmr_bentler", "srmr_bentler_nomean", "crmr",
                      "crmr_nomean", "srmr_mplus", "srmr_mplus_nomean", "ecvi")

  low_cut_index <- c("pvalue", "pvalue.scaled", "cfi", "tli", "nnfi", "rfi", "nfi", "pnfi", "ifi", "rni", "cfi.scaled", "tli.scaled", "cfi.robust", "tli.robust",
                     "nnfi.scaled", "nnfi.robust", "rfi.scaled", "nfi.scaled", "ifi.scaled", "rni.scaled", "rni.robust", "logl", "unrestricted.logl", "gfi",
                     "agfi", "pgfi", "mfi", "rmsea.pvalue", "rmsea.pvalue.scaled", "rmsea.pvalue.robust", "cn_05", "cn_01")


  for (i in 1:length_di) {
    if ((which_fit[i] %in% high_cut_index)==T) {
      fit_simresults[i,1] <- empirical_fit[which_fit[i]]
      fit_simresults[i,2] <- mean(fit_distributions[,i], na.rm = T)
      fit_simresults[i,3] <- sd(fit_distributions[,i], na.rm = T)
      fit_simresults[i,4] <- median(fit_distributions[,i], na.rm = T)
      fit_simresults[i,5] <- quantile(fit_distributions[,i], probs=(1-alpha_level), na.rm = T)
    } else if ((which_fit[i] %in% low_cut_index)==T) {
      fit_simresults[i,1] <- empirical_fit[which_fit[i]]
      fit_simresults[i,2] <- mean(fit_distributions[,i], na.rm = T)
      fit_simresults[i,3] <- sd(fit_distributions[,i], na.rm = T)
      fit_simresults[i,4] <- median(fit_distributions[,i], na.rm = T)
      fit_simresults[i,5] <- quantile(fit_distributions[,i], probs=alpha_level, na.rm = T)
    } else {
      fit_simresults[i,1] <- empirical_fit[which_fit[i]]
      fit_simresults[i,2] <- mean(fit_distributions[,i], na.rm = T)
      fit_simresults[i,3] <- sd(fit_distributions[,i], na.rm = T)
      fit_simresults[i,4] <- median(fit_distributions[,i], na.rm = T)
      fit_simresults[i,5] <- NA ####################################################################################### all NA, why?
    }
  }

  #simulation stats----------------------------------------------------------

  n_conv <- sum(!is.na(fit_distributions[,1]))
  s_est <- lavInspect(fit, what="call")$estimator
  if (length(s_est)==0) {s_est <- lavInspect(fit, what="options")$estimator}
  simulation_stats <- data.frame(matrix(c(n_rep, n_conv, s_est, alpha_level),1,4))
  names(simulation_stats)<-c("# Runs", "# Converged Runs", "Estimator", "Alpha Level")
  rownames(simulation_stats) <- ""

  #generate output---------------------------------------------------------------
  ezCutoffs_out <- list("simulation parameters"=simulation_stats, "data"=data_s_list, "fit distributions"=fit_distributions, "summary"=fit_simresults)
  class(ezCutoffs_out) <- "ezCutoffs"

  return(ezCutoffs_out)
}

a <- ezCutoffs(model=model1, data = data1, n_rep = 100)
b <- ezCutoffs(model=model2, data = data2, n_rep = 100)


#summary----------------------------------------------------------------------

summary.ezCutoffs <- function(x) {
  print(x[["simulation parameters"]])
  cat("\n")
  print(x[["summary"]])
}

summary(a)
summary(b)


#plot---------------------------------------------------------------

plot.ezCutoffs <- function(d) {

  for (i in 1:nrow(d[["summary"]])){
    hist_plots <- ggplot(d[["fit distributions"]], aes_string(x=rownames(d[["summary"]])[i]))+
      geom_histogram(aes(y=..count..),color="black", fill="white", bins = 30)

    bar_height <- max(ggplot_build(hist_plots)$data[[1]]$count)

    hist_plots <- hist_plots + geom_vline(xintercept=d[["summary"]][i,1], color="blue", alpha=0.8)+
      annotate("text", x=d[["summary"]][i,1], y = bar_height, angle=0, label="Empirical")+
      geom_vline(xintercept=d[["summary"]][i,5], color="red",alpha=0.8)+
      annotate("text", x=d[["summary"]][i,5], y = bar_height, angle=0, label="Cutoff")+
      labs(title="Simulated Fit Distribution", y = "count")+
      theme(plot.title = element_text(hjust = 0.5))
    (print(hist_plots))
  }
}

plot(a)
plot(b)

#comparison of two ezCutoffs outputs using Wilxocon-test---------------

compareFit <- function(x, y, which_fit = c("chisq", "cfi", "tli", "rmsea", "srmr"), ...) {
  
  for (i in 1:length(which_fit)) {
    name_warn <- which_fit[i]
    if (any(names(x[["fit distributions"]])==which_fit[i])==F) {
      stop("The following fit measure is not available in output 'x': ", name_warn)}
    if (any(names(y[["fit distributions"]])==which_fit[i])==F) {
      stop("The following fit measure is not available in output 'y': ", name_warn)}
  }
   
  wilc_result <- vector("list", length(which_fit))
  
  for (i in 1:length(which_fit)) {
    wilc_result[[i]] <- wilcox.test(x[["fit distributions"]][[which_fit[i]]], y[["fit distributions"]][[which_fit[i]]], ...)
    wilc_result[[i]] <- c(wilc_result[[i]], which_fit[[i]])
  }
  
  class(wilc_result) <- "wilc_result"
  return(wilc_result)
  
}


wilcox.test(a[["fit distributions"]][["chisq"]], b[["fit distributions"]][["chisq"]])

w <- compareFit(a, b)



summary.wilc_result <- function(x) {
  output <- data.frame(matrix(NA, length(x), 2))
  names(output) <- c("Wilcoxon W", "p-value")
  for (i in 1:length(x)) {
    output[i,1] <- x[[i]][["statistic"]]
    output[i,2] <- x[[i]][["p.value"]]
    rownames(output)[i] <- x[[i]][[8]]
  }
  print(output, digits=4)
}

summary(w)


#TO DO-------------------------------------------------------------------------

#plot
  #color fill cutoff bins vs non-cutoff ones
  #annotation repel, not wokring yet

#morefitmeasures
  # "object 'data_s_list' not found"
  # morefitmeasures() fits the model again internally
  # doesn't find the correct list position as it seems it isn't stored in the lavaan object

#generates perfect normal distribution, doesn't consider item scale etc...
  # kurtosis
  # skewness
  # ov.var


##############


