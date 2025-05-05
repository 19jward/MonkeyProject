\name{cov2prob}
\alias{cov2prob}
\title{turn covariates to probability}
\description{gives probabilities for each behavior given covariates}
\usage{
cov2prob(X, beta)}
\arguments{
  \item{X}{covariate dummy, FxM matrix where F is the number of focals and M is the number of covariates (including intercept)}
  \item{beta}{slice of beta transition array, BxM matrix}}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{returns a 1xB matrix (where B is the number of behaviors)}
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
## example for 5 behaviors
fake_beta_slice <- matrix(2*rnorm(5), nrow = 2, ncol = 5) 
X_test <- c(1, 1) ##intercept + 1 covariate indicator
cov2prob(X_test, fake_beta_slice)

## The function is currently defined as
function (X, beta) 
{
    eta_b1 <- X \%*\% beta
    exp_eta_b1 <- exp(eta_b1)
    P_b1 <- exp_eta_b1/rowSums(exp_eta_b1)
    return(P_b1)
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
