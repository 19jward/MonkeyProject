# for a fixed row_behavior = b1, find likelihood component
get_ll_row <- function(data, p_row, row_behavior, behavior_list){
  Yduration <- matrix(NA, nrow = 1, ncol = length(behavior_list))
  rownames(Yduration) <- row_behavior
  colnames(Yduration) <- behavior_list
  
  denominator <- sum(data$TimeSpent[data$Behavior == row_behavior]) -
    as.numeric(tail(data$Behavior, 1) == row_behavior)
  
  row_ind <- which(data$Behavior == row_behavior)
  
  for(b2 in behavior_list){
    if(row_behavior == b2){
      numerator <- denominator - sum(data$Behavior == row_behavior) + 
        as.numeric(tail(data$Behavior, 1) == row_behavior) 
    } else { 
      numerator <- sum(data$Behavior[row_ind + 1] == b2, na.rm = T)
    }
    Yduration[1,b2] <- numerator
  }
  
  LogL <- sum((log(p_row)*Yduration)[Yduration>0])
  
  if(is.na(LogL)){
    return(-Inf)
  } else {
    return(LogL)
  }
}