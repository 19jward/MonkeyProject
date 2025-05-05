\name{get_ll}
\alias{get_ll}
\title{ give log-likelihood for a dataset w/a transition matrix}
\description{
give log-likelihood for a dataset given a specified transition matrix of point estimates}
\usage{
get_ll(data, P, behavior_list)}
\arguments{
  \item{data}{a single focal of data}
  \item{P}{transition matrix for behaviors}
  \item{behavior_list}{list of present behaviors in the transition matrix}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
returns a number (log-likelihood)}
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
fake_behaviors <- c("move","feed", "move", "sleep", "feed", "sleep", "move", "sleep", "move")
fake_duration <- c(5, 10, 4, 102, 16, 58, 14, 9, 11) 
fake_data <- data.frame(fake_duration, fake_behaviors)
colnames(fake_data) <- c("TimeSpent", "Behavior")
behaviors <- c("move", "sleep", "feed")
P <- matrix(runif(9, 0, 1), nrow=3, ncol =3)
get_ll(fake_data, P, behaviors)

## The function is currently defined as
function (data, P, behavior_list) 
{
    Yduration <- matrix(NA, nrow = length(behavior_list), ncol = length(behavior_list))
    rownames(Yduration) <- colnames(Yduration) <- behavior_list
    for (b1 in behavior_list) {
        time_sum <- sum(data$TimeSpent[data$Behavior == b1]) - 
            as.numeric(tail(data$Behavior, 1) == b1)
        b1_ind <- which(data$Behavior == b1)
        for (b2 in behavior_list) {
            if (b1 == b2) {
                duration <- time_sum - sum(data$Behavior == b1) + 
                  as.numeric(tail(data$Behavior, 1) == b1)
            }
            else {
                duration <- sum(data$Behavior[b1_ind + 1] == 
                  b2, na.rm = T)
            }
            Yduration[b1, b2] <- duration
        }
    }
    LogL <- sum((log(P) * Yduration)[Yduration > 0])
    if (is.na(LogL)) {
        return(-Inf)
    }
    else {
        return(LogL)
    }
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
