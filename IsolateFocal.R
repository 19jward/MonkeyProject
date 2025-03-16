## function to isolate a single focal & add a cumulative time column
isolate.focal <- function(i){
  onefocal.i <- subset(monkeyprelimdata, monkeyprelimdata$FocalID == i)
  cumsum.i <- data.frame(cumsum(onefocal.i$TimeSpent))
  onefocal.i <- cbind(onefocal.i, cumsum.i)
  return(onefocal.i)
}