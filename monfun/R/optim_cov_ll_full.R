## function to optimize beta
## takes:
  ## data
  ## X (covariates)
  ## behavior list

## function is dependent on:
  ## optim_cov_ll_b1
  ## get_ll_cov_b1
  ## cov2prob
  ## get_ll_row

optim_cov_ll_full <- function(data, X, behavior_list){
  
  B <- length(behavior_list)
  num_cov <- ncol(X)
  
  out <- array(NA, dim = c(num_cov, B, B), dimnames = list(c("intercept", "habitat"), behavior_list, behavior_list))
  
  for (i in 1:B){
    
    b1 <- behavior_list[i]
    
    fit <- optim_cov_ll_b1(data = data, X = X, behavior_list = behavior_list,
                           beta_starting_behavior = b1, num_cov = num_cov)
    
    out[ , , i] <- fit
    
    
    
  } ## end for loop
  
  return(out) ## MxBxB
  
} ## end function