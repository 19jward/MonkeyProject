# create transition matrix

behaviorshortlist <-c("Move", "Vigilance", "Feed", "Rest",
                      "Affiliative", "Aggressive", "SelfGroom",
                      "Survey", "PAUSE", "Human Directed",
                      "Forage", "Sleep", "Play")

matrixfunct <- function(dataframe, behaviorshortlist){
  Matrix <- matrix(NA, nrow = length(behaviorshortlist), ncol = length(behaviorshortlist))
  rownames(Matrix) <- colnames(Matrix) <- behaviorshortlist
  for(b1 in behaviorshortlist){
    denomentator <- sum(dataframe$TimeSpent[dataframe$Behavior == b1]) - 
      as.numeric(tail(dataframe$Behavior, 1) == b1) ## accounting for if the last behavior in focal is b1 (doesn't count as a transition opportunity), overall total possible transition opportunities
    b1_ind <- which(dataframe$Behavior == b1)
    for(b2 in behaviorshortlist){
      if(b1 == b2){ ## if staying in the same behavior
        numerator <- denomentator - sum(dataframe$Behavior == b1) + 
          as.numeric(tail(dataframe$Behavior, 1) == b1) 
        ## removing the times they switched to a diff behavior & adding back in that situation if the last behavior is b1
      } else { ## if moving to a different behavior
        numerator <- sum(dataframe$Behavior[b1_ind + 1] == b2, na.rm = T)
      }
      Matrix[b1,b2] <- numerator/denomentator
    }
  }
  return(Matrix)
}