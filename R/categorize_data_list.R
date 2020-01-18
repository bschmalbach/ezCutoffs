categorize_data_list <- function(datalist, n_cat = 5, condition = "symmetric", rescale = T)
{
     ### ----------------- generating list --------------------------------------
     data_cat <- replicate(n = length(datalist), expr = list())
     colname <- colnames(datalist[[1]])
     
     ### ----------------- Equal or not equal categorization and number of categories ------------------
     if(length(condition) == 1 & length(n_cat) == 1) # equal categorization: ------------------------------
     {
          ### ----------------- redefining functions for easy acces ------------------
          rescale_cat_func <- function(x) # rescaling
          {
               return(categorical_data_msd(X = x, n_cat = n_cat, condition = condition, m = NULL, s = NULL))
          }
          
          simple_cat <- function(x) # no rescaling
          {
               return(categorical_data(X = x, n_cat = n_cat, condition = condition))
          }
          
          ### ----------------- Categorizing data  -----------------------------------
          for(i in 1:length(datalist))
          {
               if(rescale == T){ data_cat[[i]] <- data.frame(apply(datalist[[i]], 2, rescale_cat_func))
               }else{ data_cat[[i]] <- data.frame(apply(datalist[[i]], 2, simple_cat))}
          }
          return(data_cat)
     }else{ # UNequal categorization: ------------------------------------------------
          if(length(condition) != length(n_cat))
          {
               if(length(n_cat) == 1)       n_cat <- rep(n_cat, length(condition))
               if(length(condition) == 1)   condition <- rep(condition, length(n_cat))
          }
          colnames_data <- colnames(datalist[[1]])
          for(i in 1:length(datalist))
          {
               if(rescale == T){ 
                    d <- datalist[[i]]
                    data_cat[[i]] <- data.frame(sapply(1:dim(d)[2],  function(j) categorical_data_msd(X = d[, j], 
                                                                                           condition = condition[j], 
                                                                                           n_cat = n_cat[j],  
                                                                                           m = NULL, s = NULL)))
                    colnames(data_cat[[i]]) <- colname
               }else{
                    d <- datalist[[i]]
                    data_cat[[i]] <- data.frame(sapply(1:dim(d)[2],  function(j) categorical_data(X = d[, j], 
                                                                                       condition = condition[j], 
                                                                                       n_cat = n_cat[j])))
                    colnames(data_cat[[i]]) <- colname
               }
          }
          return(data_cat)
     }
     
}