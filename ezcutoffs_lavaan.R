library(lavaan)
library(semTools)
library(ggplot2)

#lavaan model syntax
model <- "F1 =~ a1 + a2 + a3 + a4"

#generate data
data <- simulateData(model, model.type = "cfa", sample.nobs = 1000)
#data[,(ncol(data)+1)] <- sample(c(1,2),replace = T, size=1000)
#names(data)[ncol(data)] <- "s2"
#names(data)


ezCutoffs <- function(model, 
                      data, 
                      n_rep = 1000, 
                      which_fit = c("chisq.scaled", "cfi", "tli", "gammaHat", "srmr"), 
                      estimator = "MLM", 
                      alpha_level = 0.05, 
                      group = NULL, 
                      group.equal = NULL, 
                      group.partial = NULL){
 
 
  #empirical fit
  fit <- cfa(model = model, data = data, estimator = estimator)
  fitmeasures <- fitMeasures(fit)
  fit <- cfa(model = model, data = data, estimator = "MLM") #moreFitIndices BUG!!! doesnt find estimator (argument in function)
  suppressWarnings(morefit <- moreFitIndices(fit))

  empirical_fit <- c(fitmeasures, morefit)

    
  #input
  pop_model <- fit@ParTable
  nvar <- fit@pta[["nvar"]][[1]]
  nfac <- fit@pta[["nfac"]][[1]]
  n <- fit@SampleStats@ntotal
  model_type <- fit@call[["model.type"]]
  n_sds <- qnorm(1-alpha_level/2)
  
  #progress bar
  pb <- winProgressBar(title = "progress bar", min = 0, max = n_rep, width = 300)
  
  #generate random data
  
  data_s_list <- list()
  data_s <- data
  
  for (j in 1:n_rep) {
    for (i in 1:nvar) {
      data_s <- simulateData(pop_model, model.type = model_type, sample.nobs = n) 
    }
    data_s_list[[j]]<- data_s
    Sys.sleep(0.001)
    setWinProgressBar(pb, j, title=paste( round(j/n_rep*100, 0), "% done")) #progress bar
  }
  
    
  #fit data in lavaan for fitmeasures()
  fit_s_list <- list()
  for (i in 1:n_rep) {
    fit_s <- cfa(model = model, data = data_s_list[[i]], estimator = estimator)
    fit_s_list[[i]]<-fit_s
    Sys.sleep(0.001)
    setWinProgressBar(pb, i, title=paste( round(i/n_rep*100, 0), "% done")) #progress bar
    }
  close(pb)
  
  #extract fitmeasures
  fitmeasures_s_list <- list()
  for (i in 1:n_rep) {
    fitmeasures_s <- fitmeasures(fit_s_list[[i]])
    fitmeasures_s_list[[i]] <- fitmeasures_s
  }


  
 # #fit data in lavaan for moreFitIndices()
 # fit_s_list <- list()
 # for (i in 1:n_rep) {
 #   fit_s <- cfa(model = model, data = data_s_list[[i]], estimator = "MLM")
 #   fit_s_list[[i]]<-fit_s
 # }

 # #extract morefitindices
 # morefit_s_list <- list()
 # for (i in 1:n_rep) {
 #   suppressWarnings(morefit_s <- moreFitIndices(fit_s_list[[i]])) #moreFitIndices BUG!!! doesnt find data_s_list (defined earlier)
 #   morefit_s_list[[i]]<-morefit_s
 # }

  
  
  #get fit distributions
  
  df_fitmeasures <- data.frame(matrix(NA,n_rep,length(fitmeasures_s_list[[1]])))
  #df_morefit <- data.frame(matrix(NA,n_rep,length(morefit_s_list[[1]])))
  
  for (i in 1:n_rep) {
    df_fitmeasures[i,] <- data.frame(matrix(unlist(fitmeasures_s_list[[i]]), ncol = length(fitmeasures_s_list[[i]])))
    #df_morefit[i,] <- data.frame(matrix(unlist(morefit_s_list[[i]]), ncol = length(morefit_s_list[[i]]))) 
  }
  
  fit_distributions <- (df_fitmeasures)
  #fit_distributions <- cbind(df_fitmeasures, df_morefit)
  
  ncols <- length(fitmeasures_s_list[[1]])
  #ncols <- length(fitmeasures_s_list[[1]]) + length(morefit_s_list[[1]]) 
  names(fit_distributions)[1:length(fitmeasures_s_list[[1]])] <- names(fitmeasures_s_list[[1]])
  #names(fit_distributions)[(length(fitmeasures_s_list[[1]])+1):ncols] <- names(morefit_s_list[[1]])
  
  length_di <- length(which_fit)
  d_index <- data.frame(matrix(rep(NA), n_rep, length_di))
  names(d_index)<-which_fit
  
  #retrieve requested fit measures
  for (i in 1:length_di) {
    d_index[,i] <- get(which_fit[i], fit_distributions)
  }
  
  #calculate descriptives
  
  fit_simresults <- data.frame(matrix(NA,length_di,5))
  names(fit_simresults) <- c("Empirical fit", "Simulation Mean", "Simulation SD", "Lower CI", "Upper CI")
  rownames(fit_simresults) <- which_fit
  
  for (i in 1:length_di) {
    fit_simresults[i,1] <- empirical_fit[which_fit[i]]
    fit_simresults[i,2] <- mean(d_index[,i], na.rm = T)
    fit_simresults[i,3] <- sd(d_index[,i], na.rm = T)
    fit_simresults[i,4] <- fit_simresults[i,2] - n_sds*fit_simresults[i,3]
    fit_simresults[i,5] <- fit_simresults[i,2] + n_sds*fit_simresults[i,3]
  }
  
  #BOUNDS FOR CFI, RMSEA, SRMR CI
  
  bound0 <- c("chisq", "chisq.scaled", "rmsea", "rmsea.robust", "srmr")
  
  for (i in 1:length_di) {
    if (((which_fit[i] %in% bound0)==T) & (fit_simresults[i,4]<0)) {
      fit_simresults[i,4]<-0
    }
  }
  
  bound1 <- c("cfi", "cfi.robust")
  
  for (i in 1:length_di) {
    if (((which_fit[i] %in% bound1)==T) & (fit_simresults[i,5]>1)) {
      fit_simresults[i,5]<-1
    }
  }
  
  #simulation stats
  
  n_conv <- sum(!is.na(fit_distributions[,1]))
  simulation_stats <- data.frame(matrix(c(n_rep, n_conv, estimator, alpha_level),1,4))
  names(simulation_stats)<-c("# Runs", "# Converged Runs", "Estimator", "Alpha Level")
  rownames(simulation_stats) <- ""
  
  #print results 
  print(simulation_stats)
  print(fit_simresults)
  
  #plot
  
  for (i in 1:length_di){
    hist_plots <- ggplot(d_index, aes_string(x=which_fit[i]))+
      geom_histogram(aes(y=..count..),color="black", fill="white", bins=30)+
      geom_vline(aes(xintercept=fit_simresults[i,1]), color="blue", alpha=0.8)+
      geom_vline(aes(xintercept=fit_simresults[i,4]), color="red",alpha=0.5)+
      geom_vline(aes(xintercept=fit_simresults[i,5]), color="red",alpha=0.5)+
      labs(title="Simulated Fit Distribution", y = "count")+
      theme(plot.title = element_text(hjust = 0.5))
    (print(hist_plots))
  }
  
  ezCutoff_out <- list("simulation parameters"=simulation_stats, "data"=data_s_list, "distribution of fit measures"=fit_distributions, "summary"=fit_simresults)  
  
  return(ezCutoff_out)
}

a <- ezCutoffs(model=model, data=data, estimator = "MLM", n_rep = 1000, which_fit=c("chisq","cfi.robust","rmsea.robust"))



#####################

##TO DO:

#progress bar

#scales of items 

#multiple groups and group constraints
#generate model fit and data for all groups separately, then combine and test for invariance

##############


