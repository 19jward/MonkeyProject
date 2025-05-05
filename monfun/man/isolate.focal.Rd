\name{isolate.focal}
\alias{isolate.focal}
\title{isolates a single focal of monkey dataset}
\description{
pulls out a single focal from the full monkey dataset & adds in a cumulative time column}
\usage{isolate.focal(data, i)
}
\arguments{
  \item{data}{full dataset, needs column names "FocalID", "TimeSpent"}
  \item{i}{integer identifier for focal to be isolated} }
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{ returns dataframe}
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
behaviors <- c("move", "feed", "aggressive", "vigilance", "move", "sleep", "vigilance","feed", "sleep", "move", "sleep", "move", "aggressive", "vigilance","sleep", "move", "feed", "move", "feed", "aggressive", "move", "aggressive", "move", "feed", "move", "sleep", "move", "feed", "aggressive", "feed", "move", "sleep", "move", "vigilance", "move", "vigilance", "aggressive", "move", "vigilance")
duration <- c(10, 20, 7, 10, 5, 140, 30, 25, 250, 30, 30, 60, 15, 10, 200, 23, 78, 20, 45, 90, 20, 10, 45, 45, 30, 10, 23, 12, 7, 11, 24, 230, 34, 63, 14, 210, 22, 13, 54) 
category <- c(rep("jalan", 15), rep("hutan", 11), rep("jalan", 13))
focal <- c(rep("1", 15), rep("2", 11), rep("3", 13))

fake_data <- data.frame(duration, behaviors, category, focal)
colnames(fake_data) <- c("TimeSpent", "Behavior", "Habitat", "FocalID")

isolated_focal <- isolate.focal(fake_data, 2)
isolated_focal

## The function is currently defined as
function (data, i) 
{
    onefocal.i <- subset(data, data$FocalID == i)
    cumsum.i <- data.frame(cumsum(onefocal.i$TimeSpent))
    onefocal.i <- cbind(onefocal.i, cumsum.i)
    return(onefocal.i)
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
