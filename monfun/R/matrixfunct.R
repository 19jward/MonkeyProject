matrixfunct <- function(data, behavior_list){
  Matrix <- matrix(NA, nrow = length(behavior_list), ncol = length(behavior_list))
  rownames(Matrix) <- colnames(Matrix) <- behavior_list
  for(b1 in behavior_list){
    denomentator <- sum(data$TimeSpent[data$Behavior == b1]) - 
      as.numeric(tail(data$Behavior, 1) == b1) ## accounting for if the last behavior in focal is b1 (doesn't count as a transition opportunity), overall total possible transition opportunities
    b1_ind <- which(data$Behavior == b1)
    for(b2 in behavior_list){
      if(b1 == b2){ ## if staying in the same behavior
        numerator <- denomentator - sum(data$Behavior == b1) + 
          as.numeric(tail(data$Behavior, 1) == b1) 
        ## removing the times they switched to a diff behavior & adding back in that     situation if the last behavior is b1
      } else { ## if moving to a different behavior
        numerator <- sum(data$Behavior[b1_ind + 1] == b2, na.rm = T)
      }
      Matrix[b1,b2] <- numerator/denomentator
    }
  }
  return(Matrix)
}