categorical_data_msd <- function(X, n_cat = 5, condition = "symmetric", m = NULL, s = NULL)
{
     if(is.null(m) | is.null(s))
     {
          X_ <- categorize(X = X, thresholds = cat_compute_thresholds(n_cat = n_cat, condition = condition))
          X_return <- ((X_ - mean(X_))/sd(X_))*sd(X) + mean(X)
          return(X_return)
     }else{
          X_ <- categorize(X = X, thresholds = cat_compute_thresholds(n_cat = n_cat, condition = condition))
          X_return <- ((X_ - mean(X_))/sd(X_))*s + m
          return(X_return)
     }
}