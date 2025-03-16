## note that you need the same order of behaviors that I have (probs can change that later)
behaviorshortlist <-c("Move", "Vigilance", "Feed", "Rest",
                      "Affiliative", "Aggressive", "SelfGroom",
                      "Survey", "Human Directed",
                      "Forage", "Sleep", "Play") 

## functions takes data (a focal) and P (a transition matrix)
get_ll <- function(data, P){
  B <- nrow(P) # number of behaviors
  y <- behaviorshortlist
  ymatrix <- model.matrix(~y - 1)
  
  ## build a Y Duration Matrix (which is just P without the denominator I think)
  Yduration <- matrix(NA, nrow = length(behaviorshortlist), ncol = length(behaviorshortlist))
  rownames(Yduration) <- colnames(Yduration) <- behaviorshortlist
  for(b1 in behaviorshortlist){
    denomentator <- sum(data$TimeSpent[data$Behavior == b1]) - 
      as.numeric(tail(data$Behavior, 1) == b1)
    b1_ind <- which(data$Behavior == b1)
    for(b2 in behaviorshortlist){
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