## collapse the behavior list and add in a duration column
## may not be actually realistic depending on what you made the fake beta
collapse_sim_data <- function(data){
  points <- which(diff(as.numeric(data$Behavior))!= 0) ## where the transitions happen
  TimeSpent <- diff(c(0, points))
  Behavior <- data$Behavior[points]
  
  out <- data.frame(Behavior, TimeSpent)
  return(out)
}