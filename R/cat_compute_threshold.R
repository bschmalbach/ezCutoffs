cat_compute_thresholds <- function(n_cat, condition = "symmetric")
{
     if(n_cat > 7 | is.na(n_cat)) return(NULL)
     
     # Symmetric Condition Thresholds in List
     Symmetric <- list()
     Symmetric$kat2 <- c(0)
     Symmetric$kat3 <- c(-0.83, 0.83)
     Symmetric$kat4 <- c(-1.25, 0, 1.25)
     Symmetric$kat5 <- c(-1.5, -0.5, 0.5, 1.5)
     Symmetric$kat6 <- c(-1.6, -0.83, 0, 0.83, 1.6)
     Symmetric$kat7 <- c(-1.79, -1.07, -0.36, 0.36, 1.07, 1.79)
     
     # Moderate Asymetrical Condition Thresholds in List
     Mod.Asym <- list()
     Mod.Asym$kat2 <- c(0.36)
     Mod.Asym$kat3 <- c(-0.5, 0.76)
     Mod.Asym$kat4 <- c(-0.31, 0.79, 1.66)
     Mod.Asym$kat5 <- c(-0.7, 0.39, 1.16, 2.05)
     Mod.Asym$kat6 <- c(-1.05, 0.08, 0.81, 1.44, 2.33)
     Mod.Asym$kat7 <- c(-1.43,-0.43, 0.38, 0.94, 1.44, 2.54)
     
     # Extremely Asymetrical Condition Thresholds in List
     Ext.Asym <- list()
     Ext.Asym$kat2 <- c(1.04)
     Ext.Asym$kat3 <- c(0.58, 1.13)
     Ext.Asym$kat4 <- c(0.28, 0.71, 1.23)
     Ext.Asym$kat5 <- c(0.05, 0.44, 0.84, 1.34)
     Ext.Asym$kat6 <- c(-0.13, 0.25, 0.61, 0.99, 1.48)
     Ext.Asym$kat7 <- c(-0.25, 0.13, 0.47, 0.81, 1.18, 1.64)
     
     
     if(condition == "symmetric") return(Symmetric[[n_cat - 1]])
     if(condition == "mod.asym") return(Mod.Asym[[n_cat - 1]])
     if(condition == "ext.asym") return(Ext.Asym[[n_cat - 1]])
     if(condition == "mod.asym-alt") return(sort(-1*Mod.Asym[[n_cat - 1]]))
     if(condition == "ext.asym-alt") return(sort(-1*Ext.Asym[[n_cat - 1]]))
}