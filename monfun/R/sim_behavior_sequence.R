## X is an FxM matrix (indicator for covariates) - for this it only works for 1 focal, so 1xM
## beta is full MxBxB array
## T_max gives a stopping point for how long the focal is
## currently specifically built for 3 behaviors, can be modified for more
## outputs list of behaviors, each assumed to be 1 unit (second) of duration

sim_behavior_sequence <- function(X, beta, T_max = 600) {
  if (is.null(dim(X))) {
    X <- matrix(X, nrow = 1)
  } ## make it work with only 1 focal
  M <- ncol(X) ## num covariates + intercept
  B <- dim(beta)[2] ## num behavior
  
  behavior_list <- dimnames(beta)[2] 
  
  out <- list()
  
  trans_mat <- matrix(0, nrow = B, ncol = B)
  for (b in 1:B) {
    trans_mat[b,] <- cov2prob(X, beta[,b,])
  }
  
  time <- 0 ; Behavior <- c()
  
  current_behavior <- sample(1:B, 1)
  
  for(i in 2:T_max) {
    Behavior <- c(Behavior, current_behavior)
    probs <- trans_mat[current_behavior, ]
    current_behavior <- sample(1:B, 1, prob = probs)
  } ## end for loop
  
  
  out <- data.frame(Behavior)
  
  out <- as.data.frame(out)
  out$Behavior <- replace(out$Behavior, out$Behavior == 1, "move")
  out$Behavior <- replace(out$Behavior, out$Behavior == 2, "sleep")
  out$Behavior <- replace(out$Behavior, out$Behavior == 3, "feed")
  out$Behavior <- as.factor(out$Behavior)
  
  return(out)
} ## end function