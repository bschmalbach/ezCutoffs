sampleData <- function(data, n_groups, n_obs, n_rep) {
  message("Sampling from Population\n")
  
  # progress bar
  pb <- progress::progress_bar$new(
    format = "  |:bar| :percent elapsed = :elapsed  ~ :eta",
    total = n_rep, complete = "=", incomplete = " ", current = " ",
    width = 80, clear = F, show_after = 0
  )
  progress <- function(n) {
    pb$tick(tokens = list(trial = (1:n_rep)[n])) # token reported in progress bar
  }
  
  if (n_groups > 1) {
    replic <- function() {
      progress()
      data %>% group_by(group) %>% sample_n(size = n_obs)    
    }
  } else {
    replic <- function() {
      progress()
      sample_n(data, size = n_obs)
    }
  }
  
  data_s_list <- vector("list", n_rep)
  data_s_list <- replicate(n_rep, replic(), simplify = F)
  
  return(data_s_list)
}