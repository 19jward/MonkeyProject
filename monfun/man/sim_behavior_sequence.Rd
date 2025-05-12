\name{sim_behavior_sequence}
\alias{sim_behavior_sequence}
\title{
simulate sequence of behaviors based on a beta probability arrray}
\description{
generates a list of sequential behaviors, currently specifically built for 3 behaviors, can be modified for more. to be used with collapse_sim_data and full_sim_data}
\usage{
sim_behavior_sequence(X, beta, T_max)
}
\arguments{
  \item{X}{X is an FxM matrix (indicator for covariates) - for this it only works for 1 focal, so 1xM}
  \item{beta}{beta is full MxBxB array}
  \item{T_max}{T_max gives a stopping point for how long the focal is (# seconds)}
} 
\details{
}
\value{
outputs list of behaviors, each assumed to be 1 unit (second) of duration
}
\references{
}
\author{
Josephine Ward}
\note{
}


\seealso{
}
\examples{
X_test <- c(1,1) ## in habitat 1
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
sim_behavior_sequence(X_test, beta_test)


## The function is currently defined as
sim_behavior_sequence <- function(X, beta, T_max = 600) {
  if (is.null(dim(X))) {
    X <- matrix(X, nrow = 1)
  } ## make it work with only 1 focal
  M <- ncol(X) ## num covariates + intercept
  B <- dim(beta)[2] ## num behavior
  
  behavior_list <- dimnames(beta)[2] 
  
  out <- list()
  
  trans_mat <- matrix(0, nrow = B, ncol = B)
  for (b in 1:B) {
    trans_mat[b,] <- cov2prob(X, beta[,b,])
  }
  
  time <- 0 ; Behavior <- c()
  
  current_behavior <- sample(1:B, 1)
  
  for(i in 2:T_max) {
    Behavior <- c(Behavior, current_behavior)
    probs <- trans_mat[current_behavior, ]
    current_behavior <- sample(1:B, 1, prob = probs)
  } ## end for loop
  
  
  out <- data.frame(Behavior)
  
  out <- as.data.frame(out)
  out$Behavior <- replace(out$Behavior, out$Behavior == 1, "move")
  out$Behavior <- replace(out$Behavior, out$Behavior == 2, "sleep")
  out$Behavior <- replace(out$Behavior, out$Behavior == 3, "feed")
  out$Behavior <- as.factor(out$Behavior)
  
  return(out)
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
