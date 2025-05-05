##optimize the beta for starting behavior i

## function takes
## data
## X
## behavior list (check caps)
## starting behavior
## number of covariates
## par = initial betas (MxB matrix) - slice of the array

optim_cov_ll_b1 <- function(data, X, behavior_list, beta_starting_behavior, num_cov, 
                            maxit = 5000){
  num_cov <- as.numeric(num_cov)
  B <- as.numeric(length(behavior_list))
  
  ## out is the beta that is optimized, this is still just working with the beta slice so should be MxB
  out <- matrix(NA, nrow = num_cov, ncol = B)
  
  starting_point <- matrix(num_cov*rnorm(B), nrow = num_cov, ncol = B) ## mimic the beta slice
  
  ## i am an optim function h8er
  fit <- optim(par = starting_point, fn = function(par, data){ 
    
    beta_start <- matrix(par, nrow = num_cov, ncol = B)
    
    get_ll_cov_b1(data = data, beta = beta_start, beta_starting_behavior = beta_starting_behavior, 
                  behavior_list = behavior_list, X = X)}, data = data, 
    control = c(fnscale = -1, maxit = maxit))
  
  ## end fit def
  
  out <- matrix(fit$par, nrow = num_cov, ncol = B)
  
  return(out)
} ## end function