## function to isolate a single focal & add a cumulative time column
## only really works with my specific data & names
## (useful for testing functions, not really for any actual analysis)

isolate.focal <- function(data, i){
  onefocal.i <- subset(data, data$FocalID == i)
  cumsum.i <- data.frame(cumsum(onefocal.i$TimeSpent))
  onefocal.i <- cbind(onefocal.i, cumsum.i)
  return(onefocal.i)
}