\name{matrixfunct}
\alias{matrixfunct}
\title{create transition matrix}
\description{creates a point estimate transition matrix (specifically for the monkey project) }
\usage{matrixfunct(data, behavior_list)}
\arguments{
  \item{data}{takes a single focal of data, needs to have column names "Behavior" and "TimeSpent"}
  \item{behavior_list}{list of behaviors present in data} }
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{a matrix of numbers (transition point probabilities)}
\references{
%% ~put references to the literature/web site here ~
}
\author{Josephine Ward}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## create fake data
fake_behaviors <- c("move","feed", "move", "sleep", "feed", "sleep", "move", "sleep", "move")
fake_duration <- c(5, 10, 4, 102, 16, 58, 14, 9, 11) 
fake_data <- data.frame(fake_duration, fake_behaviors)
colnames(fake_data) <- c("TimeSpent", "Behavior")
behaviors <- c("move", "sleep", "feed")
matrixfunct(fake_data, behaviors)



## The function is currently defined as
matrixfunct <- function(data, behavior_list){
  Matrix <- matrix(NA, nrow = length(behavior_list), ncol = length(behavior_list))
  rownames(Matrix) <- colnames(Matrix) <- behavior_list
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
      Matrix[b1,b2] <- numerator/denomentator
    }
  }
  return(Matrix)
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
