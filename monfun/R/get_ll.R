## initially adapted from the Transition Matrix function we made
get_ll <- function(data, P, behavior_list){
  ## build a Y Duration Matrix (P without the denominator)
  Yduration <- matrix(NA, nrow = length(behavior_list), ncol = length(behavior_list))
  rownames(Yduration) <- colnames(Yduration) <- behavior_list
  for(b1 in behavior_list){
    time_sum <- sum(data$TimeSpent[data$Behavior == b1]) - 
      as.numeric(tail(data$Behavior, 1) == b1)
    b1_ind <- which(data$Behavior == b1)
    for(b2 in behavior_list){
      if(b1 == b2){
        duration <- time_sum - sum(data$Behavior == b1) + 
          as.numeric(tail(data$Behavior, 1) == b1) 
      } else { 
        duration <- sum(data$Behavior[b1_ind + 1] == b2, na.rm = T)
      }
      Yduration[b1,b2] <- duration
    }
  }
  
  LogL <- sum((log(P)*Yduration)[Yduration>0])
  
  if(is.na(LogL)){
    return(-Inf)
  } else {
    return(LogL)
  }
}
