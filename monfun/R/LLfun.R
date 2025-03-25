get_ll <- function(data, P, behavior_list){
  B <- nrow(P) # number of behaviors
  y <- behavior_list
  ymatrix <- model.matrix(~y - 1)
  
  ## build a Y Duration Matrix (which is just P without the denominator I think)
  Yduration <- matrix(NA, nrow = length(behavior_list), ncol = length(behavior_list))
  rownames(Yduration) <- colnames(Yduration) <- behavior_list
  for(b1 in behavior_list){
    denomentator <- sum(data$TimeSpent[data$Behavior == b1]) - 
      as.numeric(tail(data$Behavior, 1) == b1)
    b1_ind <- which(data$Behavior == b1)
    for(b2 in behavior_list){
      if(b1 == b2){
        numerator <- denomentator - sum(data$Behavior == b1) + 
          as.numeric(tail(data$Behavior, 1) == b1) 
      } else { 
        numerator <- sum(data$Behavior[b1_ind + 1] == b2, na.rm = T)
      }
      Yduration[b1,b2] <- numerator
    }
  }
  
  LogL <- sum((log(P)*Yduration)[Yduration>0])
  
  if(is.na(LogL)){
    return(-Inf)
  } else {
    return(LogL)
  }
}