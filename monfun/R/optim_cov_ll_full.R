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
  if (is.null(dim(X))) {
    X <- matrix(X, nrow = 1)
  }
  B <- length(behavior_list)
  num_cov <- ncol(X)
  
  row_names <- c("intercept", "habitat")
  out <- array(NA, dim = c(num_cov, B, B), dimnames = list(row_names, behavior_list, behavior_list))
  
  for (i in 1:B){
    
    b1 <- behavior_list[i]
    
    fit <- optim_cov_ll_b1(data = data, X = X, behavior_list = behavior_list,
                           beta_starting_behavior = b1, num_cov = num_cov)
    
    out[ , , i] <- fit
    
    
    
  } ## end for loop
  
  return(out) ## MxBxB
  
} ## end function
