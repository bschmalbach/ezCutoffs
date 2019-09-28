library(lavaan)
library(semTools)
library(ggplot2)
library(ggrepel)
library(pbapply)

#lavaan model syntax
model <- "F1 =~ a1 + a2 + a3 + a4 + a5"

#generate data
data <- simulateData(model, model.type = "cfa", sample.nobs = 1000)
#for multigroup cfa
#data[,(ncol(data)+1)] <- sample(c(1,2),replace = T, size=1000)
#names(data)[ncol(data)] <- "s2"
#names(data)



ezCutoffs <- function(model, 
                      data, 
                      n_rep = 1000, 
                      which_fit = c("chisq", "cfi", "tli", "rmsea", "srmr"), 
                      estimation = NULL, 
                      alpha_level = 0.05, 
                      group = NULL, 
                      group.equal = NULL, 
                      group.partial = NULL,
                      std_err = "none"){
 
  #empirical fit
  if (is.null(estimation)) {
    fit <- cfa(model = model, data = data, se = std_err)
  } else {
    fit <- cfa(model = model, data = data, estimator = estimation, se = std_err)  
  }
  
  fit_measures <- fitMeasures(fit)
  #fit <- cfa(model = model, data = data, estimator = estimation) #moreFitIndices BUG!!! doesnt find estimation (argument in function)
  #suppressWarnings(more_fit <- moreFitIndices(fit))

  empirical_fit <- fit_measures
  #empirical_fit <- c(fit_measures, more_fit)

  #sanity check: which_fit in empirical fit?
  length_di <- length(which_fit)
  for (i in 1:length_di) {
    name_warn <- which_fit[i]
    if (which_fit[i] %in% names(empirical_fit) == F) {stop(c("The following fit measure is not available with the given parameters: ", name_warn))}
  }
  
  #input parameters
  pop_model <- parTable(fit)
  n <- vartable(fit)[1,"nobs"]
  model_type <- fit@call[["model.type"]] #where can i get model.type, or does it even matter, just enter "cfa"?

  #generate random data

  cat("\nData Generation\n")
  
  data_s_list <- vector("list", n_rep) 
  data_generation <- function() {
    as.data.frame(simulateData(pop_model, model.type = model_type, sample.nobs = n))
  }
  data_s_list <- pbreplicate(n_rep, data_generation(), simplify = F)
  
  #fit data in lavaan for fitmeasures()
  cat("\nModel Fitting\n")
  
  pb <- timerProgressBar(max = n_rep, char = "+", width = 50)
  
  if (is.null(estimation)) {
    #with standard esimator
    fit_s_list <- vector("list", length = n_rep)
    for (i in 1:n_rep) {
      fit_s <- cfa(model = model, data = data_s_list[[i]], se = std_err)
      fit_s_list[[i]]<-fit_s
      setTimerProgressBar(pb, i)
    }
  } else {
    #with non-standard esimator
    fit_s_list <- vector("list", length = n_rep)
    for (i in 1:n_rep) {
      fit_s <- cfa(model = model, data = data_s_list[[i]], estimator = estimation, se = std_err)
      fit_s_list[[i]]<-fit_s
      setTimerProgressBar(pb, i)
    }
  }
  
  closepb(pb)
  
  #extract fit_measures
  fit_measures_s_list <- list()
  for (i in 1:n_rep) {
    fit_measures_s <- fitmeasures(fit_s_list[[i]], fit.measures = which_fit)
    fit_measures_s_list[[i]] <- fit_measures_s
  }

  
 # #fit data in lavaan for moreFitIndices()
 # fit_s_list <- list()
 # for (i in 1:n_rep) {
 #   fit_s <- cfa(model = model, data = data_s_list[[i]], estimator = estimation)
 #   fit_s_list[[i]]<-fit_s
 # }

 # #extract morefitindices
 # more_fit_s_list <- list()
 # for (i in 1:n_rep) {
 #   suppressWarnings(more_fit_s <- moreFitIndices(fit_s_list[[i]])) #moreFitIndices BUG!!! doesnt find data_s_list (defined earlier)
 #   more_fit_s_list[[i]]<-more_fit_s
 # }

  
  #get fit distributions
  
  m_fit_measures <- matrix(NA,n_rep,length(fit_measures_s_list[[1]]))
  #m_more_fit <- matrix(NA,n_rep,length(more_fit_s_list[[1]])))
  
  for (i in 1:n_rep) {
    m_fit_measures[i,] <- matrix(unlist(fit_measures_s_list[[i]]), ncol = length(fit_measures_s_list[[i]]))
    #m_more_fit[i,] <- matrix(unlist(more_fit_s_list[[i]]), ncol = length(more_fit_s_list[[i]]))) 
  }
  
  fit_distributions <- as.data.frame(m_fit_measures)
  #fit_distributions <- cbind(m_fit_measures, m_more_fit)
  
  #ncols <- length(fit_measures_s_list[[1]])
  #ncols <- length(fit_measures_s_list[[1]]) + length(more_fit_s_list[[1]]) 
  names(fit_distributions)[1:length(fit_measures_s_list[[1]])] <- names(fit_measures_s_list[[1]])
  #names(fit_distributions)[(length(fit_measures_s_list[[1]])+1):ncols] <- names(more_fit_s_list[[1]])
  
  
  #calculate descriptives
  fit_simresults <- data.frame(matrix(NA,length_di,5))
  percentage_a <- 100*alpha_level
  cutoff_name <- paste(percentage_a, "% Cutoff", collapse = "", sep = "")
  names(fit_simresults) <- c("Empirical fit", "Simulation Mean", "Simulation SD", "Simulation Median", cutoff_name)
  rownames(fit_simresults) <- which_fit
  
  
  #Upper or Lower Cut
  
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
  
  #simulation stats
  
  n_conv <- sum(!is.na(fit_distributions[,1]))
  if (is.null(estimation)) {
  simulation_stats <- data.frame(matrix(c(n_rep, n_conv, "ML", alpha_level),1,4)) ####### how to tell which estimation method was used by lavaan without @?
  } else {
  simulation_stats <- data.frame(matrix(c(n_rep, n_conv, estimation, alpha_level),1,4))
  }
  names(simulation_stats)<-c("# Runs", "# Converged Runs", "Estimator", "Alpha Level")
  rownames(simulation_stats) <- ""
  
  
  ezCutoffs_out <- list("simulation parameters"=simulation_stats, "data"=data_s_list, "fit distributions"=fit_distributions, "summary"=fit_simresults)  
  class(ezCutoffs_out) <- "ezCutoffs"
  
  return(ezCutoffs_out)
}

a <- ezCutoffs(model=model, data=data, estimation = "MLM", n_rep = 100, which_fit=c("chisq","cfi","rmsea"))


#define summary

summary.ezCutoffs <- function(x) {
  print(x[["simulation parameters"]])
  cat("\n")
  print(x[["summary"]])
}

#print simulation + fit results
summary(a)



#define plot
#################################################################################TODO
plot.ezCutoffs <- function(d) {
  
  for (i in 1:nrow(d[["summary"]])){
    hist_plots <- ggplot(d[["fit distributions"]], aes_string(x=rownames(d[["summary"]])[i]))+
      geom_histogram(aes(y=..count..),color="black", fill="white", bins = 30)
    
    bar_height <- max(ggplot_build(hist_plots)$data[[1]]$count)
          
    hist_plots <- hist_plots + geom_vline(xintercept=d[["summary"]][i,1], color="blue", alpha=0.8)+
      annotate("text_repel", x=d[["summary"]][i,1], y = bar_height, angle=45, label="Empirical")+
      geom_vline(xintercept=d[["summary"]][i,5], color="red",alpha=0.8)+
      annotate("text_repel", x=d[["summary"]][i,5], y = bar_height, angle=45, label="Cutoff")+
      labs(title="Simulated Fit Distribution", y = "count")+
      theme(plot.title = element_text(hjust = 0.5))
    (print(hist_plots))
  }
}

#plot
plot(a)


#####################

##TO DO:

#plot: color fill cutoff bins

#pass model type + estimation method for simulation

#pass lavaanoptions (such as group etc...)

#generates perfect normal distribution, doesn't consider item scale etc...

#multiple groups and group constraints
#generate model fit and data for all groups separately, then combine and test for invariance
#by defining in model syntax c(a1, a1)?

##############

