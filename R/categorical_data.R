categorical_data <- function(X, n_cat, condition = "symmetric")
{
     return(categorize(X = X, thresholds = cat_compute_thresholds(n_cat = n_cat, condition = condition)))
}