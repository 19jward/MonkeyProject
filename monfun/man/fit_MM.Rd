\name{fit_MM}
\alias{fit_MM}
\title{optimize LL transition matrix}
\description{
get optimized transition matrix for a focal based on log-likelihood. note this is currently only accurate for up to 5 behaviors}
\usage{
fit_MM(data, behavior_list, par = matrix(1/length(behavior_list), nrow = length(behavior_list), ncol = length(behavior_list) - 1), maxit = 5000, rowwise = T)
}
\arguments{
  \item{data}{single focal of data from the monkey dataset}
  \item{behavior_list}{list of present behaviors}
  \item{par}{starting transition matrix to be optimized (default is uniform), minus the last column}
  \item{maxit}{maximum number of iterations to try to converge to optim}
  \item{rowwise}{calculate by row or all at once (row recommended)}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{returns a transition matrix}
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
fake_behaviors <- c("move","feed","move", "sleep", "feed", "sleep", "move", "sleep", "move")
fake_duration <- c(5, 10, 4, 102, 16, 58, 14, 9, 11) 
fake_data <- data.frame(fake_duration, fake_behaviors)
colnames(fake_data) <- c("TimeSpent", "Behavior")
behaviors <- c("move", "sleep", "feed")
fit_MM(fake_data, behaviors)

## The function is currently defined as
function (data, behavior_list, par = matrix(1/length(behavior_list), 
    nrow = length(behavior_list), ncol = length(behavior_list) - 
        1), maxit = 5000, rowwise = T) 
{
    if (!isTRUE(rowwise)) {
        fit <- optim(par = par, fn = function(par, data) {
            if (any(par < 0)) 
                return(-Inf)
            if (any(par > 1)) 
                return(-Inf)
            Pf <- matrix(par, nrow = length(behavior_list))
            if (any(rowSums(Pf) > 1)) 
                return(-Inf)
            Pf <- cbind(Pf, 1 - rowSums(Pf))
            rownames(Pf) <- colnames(Pf) <- behavior_list
            get_ll(data, Pf, behavior_list)
        }, data = data, control = c(fnscale = -1, maxit = maxit))
        out <- fit$par
        out <- cbind(out, 1 - rowSums(out))
        rownames(out) <- colnames(out) <- behavior_list
        return(out)
    }
    else if (isTRUE(rowwise)) {
        out <- matrix(NA, nrow = length(behavior_list), ncol = length(behavior_list))
        rownames(out) <- colnames(out) <- behavior_list
        rownames(par) <- behavior_list
        for (b1 in behavior_list) {
            par_row <- par[b1, ]
            fit <- optim(par = par_row, fn = function(par, data) {
                if (any(par < 0)) 
                  return(-Inf)
                if (any(par > 1)) 
                  return(-Inf)
                if (sum(par) > 1) 
                  return(-Inf)
                p_row <- c(par, 1 - sum(par))
                names(p_row) <- behavior_list
                get_ll_row(data, p_row, row_behavior = b1, behavior_list)
            }, data = data, control = c(fnscale = -1, maxit = maxit))
            row_out <- fit$par
            row_out <- c(row_out, 1 - sum(row_out))
            out[b1, ] <- row_out
        }
        return(out)
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
