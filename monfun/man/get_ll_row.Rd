\name{get_ll_row}
\alias{get_ll_row}
\title{for a fixed row_behavior = b1, find likelihood component}
\description{finds log-likelihood for a single behavior in the data: used for more efficient and accurate optimizing later}
\usage{
get_ll_row(data, p_row, row_behavior, behavior_list)
}
\arguments{
  \item{data}{a single focal of data}
  \item{p_row}{specific row of transition matrix for the specific behavior}
  \item{row_behavior}{which behavior the row refers to}
  \item{behavior_list}{list of behaviors present in data} }
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{returns a number (log-likelihood)}
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
p_row <- matrix(runif(3, 0, 1), nrow=1, ncol =3)

get_ll_row(fake_data, p_row, "move", behaviors)

## The function is currently defined as
function (data, p_row, row_behavior, behavior_list) 
{
    Yduration <- matrix(NA, nrow = 1, ncol = length(behavior_list))
    rownames(Yduration) <- row_behavior
    colnames(Yduration) <- behavior_list
    time_sum <- sum(data$TimeSpent[data$Behavior == row_behavior]) - 
        as.numeric(tail(data$Behavior, 1) == row_behavior)
    row_ind <- which(data$Behavior == row_behavior)
    for (b2 in behavior_list) {
        if (row_behavior == b2) {
            duration <- time_sum - sum(data$Behavior == row_behavior) + 
                as.numeric(tail(data$Behavior, 1) == row_behavior)
        }
        else {
            duration <- sum(data$Behavior[row_ind + 1] == b2, 
                na.rm = T)
        }
        Yduration[1, b2] <- duration
    }
    LogL <- sum((log(p_row) * Yduration)[Yduration > 0])
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
