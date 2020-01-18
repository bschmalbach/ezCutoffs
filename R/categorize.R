categorize <- function(X, thresholds)
{
     if(is.null(thresholds)) return(X) # if thresholds are NULL, X will not be categorized
     # deal with thresholds
     thresholds <- sort(thresholds)
     if(!(-Inf %in% thresholds)) thresholds <- c(-Inf, thresholds)
     if(!(Inf %in% thresholds)) thresholds <- c(thresholds, Inf)
     
     # define number and labels of categories 
     n_kat <- length(thresholds)-1
     if((n_kat %% 2) != 0)
     {
          categories <-  -((n_kat-1)/2):((n_kat-1)/2)
     }else{
          categories <-  seq(-((n_kat-1)/2),((n_kat-1)/2),1)
     }
     
     # standardize X
     X_std <- (X-mean(X))/sd(X)
     
     X_cat <- as.numeric(cut(x = X_std, breaks = thresholds, ordered_result = T)) - (n_kat+1)/2
     return(X_cat)
}