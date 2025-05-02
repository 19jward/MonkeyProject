## cov2prob function
## takes X, beta, and a behavior list
## X is FxM matrix (F = # focals, M = # predictors including intercept)
## beta is MxB matrix (B = # behaviors)

## this function doesnt take data!

## this function is for a single initial behavior, will loop over it in a new get_ll fnctn
cov2prob <- function(X, beta){
  
  eta_b1 <- X%*%beta  ## FxB matrix
  exp_eta_b1 <- exp(eta_b1)
  P_b1 <- exp_eta_b1/rowSums(exp_eta_b1)
  
  return(P_b1)
}