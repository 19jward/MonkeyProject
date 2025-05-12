\name{collapse_sim_data}
\alias{collapse_sim_data}
\title{
collapse simulated data
}
\description{
takes generated sequence of behaviors, collapses the repeats and adds in the duration column, to be used after sim_behavior_sequence and in full_sim_data}
\usage{
collapse_sim_data(data)
}
\arguments{
  \item{data}{sequence of behaviors}
}
\details{}
\value{returns data frame with one column of behaviors and one column of duration (TimeSpent)}
\references{
}
\author{
Josephine Ward}
\note{
}

\seealso{
}
\examples{
## create fake beta --> 3 behaviors (B), 1 predictor + intercept (M)
row_names <- c("intercept", "habitat")
col_names <- c("move", "sleep", "feed")
mat_names <- c("move", "sleep", "feed")
beta_test <- array(0, dim = c(2, 3, 3), dimnames = list(row_names, col_names, mat_names))
B <- 3
beta_test[1, , ] <- 2*diag(B)
M <- 2
for (m in 2:M) {
  mat <- matrix(runif(B^2), nrow = B)
  beta_test[m,,] <- mat/rowSums(mat)
}
X_test <- c(1,1) ## in habitat 1
blist <- c("move", "sleep", "feed")

test <- sim_behavior_sequence(X_test, beta_test)
test <- collapse_sim_data(test)


## The function is currently defined as
collapse_sim_data <- function(data){
  points <- which(diff(as.numeric(data$Behavior))!= 0) ## where the transitions happen
  TimeSpent <- diff(c(0, points))
  Behavior <- data$Behavior[points]
  
  out <- data.frame(Behavior, TimeSpent)
  return(out)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
