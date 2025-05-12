\name{full_sim_data}
\alias{full_sim_data}
\title{
generates a simulated data set for monkey project}
\description{
uses sim_behavior_sequence and collapse_sim_data to output a full simulated data frame of focals}
\usage{
full_sim_data(X, beta, T_max, num_focals)
}
\arguments{
  \item{X}{FxM matrix of indicators for covariates (F = # focals, M = # covariates)}
  \item{beta}{full MxBxB array}
  \item{T_max}{max number of seconds for each focal}
  \item{num_focals}{integer number of focals to include in output}
}
\details{
}
\value{
outputs data frame with following columns: Behavior, TimeSpent, Habitat, FocalID
}
\references{
}
\author{
Josephine Ward}
\note{this function is still under construction}
\seealso{}
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
X_test2 <- matrix(c(1, 1, 1, 1, 0, 1), nrow = 3, ncol = 2) ## multiple focal X

test_data <- full_sim_data(X_test2, beta_test, T_max = 1000, num_focals = 3)

## The function is currently defined as
full_sim_data <- function(X, beta, T_max, num_focals){
  output <- c()
  for (i in 1:num_focals){
    x_i <- X[i, ] ## pull out correct focal from X
    if (is.null(dim(x_i))) {
      x_i <- matrix(x_i, nrow = 1)
    }
    
    ## run the simulate behaviors
    data <- sim_behavior_sequence(x_i, beta, T_max)
    
    ## collapse the simulated behaviors
    data <- collapse_sim_data(data)
    
    ## add FocalID column
    FocalID <- c(rep.int(i, nrow(data)))
    data <- cbind(data, FocalID)
    
    ## add habitat column
    Habitat <- c(rep.int(x_i[1,2], nrow(data)))
    data <- cbind(data, Habitat)
    
    ## put multiple focals together
    output <- rbind(output, data)
    
  } ## end focal for loop
  
  output$Habitat <- replace(output$Habitat, output$Habitat == 0, "jalan")
  output$Habitat <- replace(output$Habitat, output$Habitat == 1, "hutan")
  
  return(output)  
} ## end function

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
