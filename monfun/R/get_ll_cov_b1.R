## getting LL across all focals given a starting behavior

## note data needs setup with column names "FocalID" and "Behavior"

## function takes:
## data (focals)
## beta (slice of BxMxM array pertaining to starting behavior)
## starting behavior 
## behavior list
## X (indicators for covariates)

get_ll_cov_b1 <- function(data, beta, beta_starting_behavior, behavior_list, X){
  num_foc <- length(unique(data$FocalID)) ## number of focals (F)
  B <- length(unique(data$Behavior)) ## number of behaviors
  
  sum_LL <- numeric(0)
  data$FocalID <- as.numeric(data$FocalID)
  
  
  P <- cov2prob(X = X, beta = beta) ## beta currently defined outside of the function 
  
  for (f in 1:num_foc){
    focal <- f
    data_focal <- subset(data, data$FocalID == f, select = c(TimeSpent, Behavior, Habitat)) 
    
    P_f <- P[f,]
    
    focal_LL <- get_ll_row(data = data_focal, p_row = P_f, row_behavior = beta_starting_behavior,
                           behavior_list = behavior_list) 
    
    sum_LL <- sum(sum_LL, focal_LL)
    
  } ## end focal loop
  
  return(sum_LL)
  
} ## end function