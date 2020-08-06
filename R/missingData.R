missingData <- function(simu, fit, data, n_rep, missing_data, dots) {
  
  message('\nIncorporating Missing Data\n')
  
  if (!any(is.na(data))) {
    missing_data <- F
  } #check that there actually is missing data, if not switch to complete
  if (missing_data == T) {
    var_table <- lavaan::varTable(fit)
    
    missings <- is.na(data)
    n_var <- nrow(var_table)
    
    if (is.null(dots$missing)==T) {
      dots$missing <- "listwise"
    }
    if (grepl('fiml', dots$missing, ignore.case = TRUE)) { #full misses
      misses <- rowSums(missings) == n_var
    } else { # listwise, any misses
      misses <- rowSums(missings) > 0
    }
    
    missings <- missings[!misses, ]
  
    # progress bar
    pb <- progress::progress_bar$new(
      format = "  |:bar| :percent elapsed = :elapsed  ~ :eta",
      total = n_rep, complete = "=", incomplete = " ", current = " ",
      width = 80, clear = F, show_after = 0
    )
    progress <- function(n) {
      pb$tick(tokens = list(trial = (1:n_rep)[n])) # token reported in progress bar
    }
    
    # single replication
    replic <- function(repl_data) {
      progress()
      repl_data[missings] <- NA
      return(repl_data)
    }
    
    simu <- lapply(simu, replic)
  }    
  
  return(simu)
}
