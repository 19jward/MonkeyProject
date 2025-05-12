\name{optim_cov_ll_b1}
\alias{optim_cov_ll_b1}
\title{
optimize beta slice by ll for a given starting behavior
}
\description{
optimize beta slices (with covariates) by log-likelihood for a given starting behavior (b1). uses cov2prob and get_ll_cov_b1. used in optim_cov_ll_full. }
\usage{
optim_cov_ll_b1(data, X, behavior_list, beta_starting_behavior, num_cov, maxit = 5000)
}
\arguments{
  \item{data}{focals, note the data must have column names "FocalID", "Behavior", "TimeSpent", & "Habitat"}
  \item{X}{indicator for covariates (matrix)} 
  \item{behavior_list}{list of behaviors in data}
  \item{beta_starting_behavior}{b1 - behavior starting from (string) }
  \item{num_cov}{number of covariates (including the intercept) (M)}
  \item{maxit}{max number of iterations to run to reach optim}
}
\details{
}
\value{
returns MxB matrix
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
fake_behaviors5.2 <- c("move", "feed", "aggressive", "vigilance", "move", "sleep", 
                     "vigilance","feed", "sleep", "move", "sleep", "move", "aggressive", "vigilance","sleep", "move", "feed", "move", "feed", "aggressive", "move", "aggressive", "move", "feed", "move", "sleep", "move", "feed", "aggressive", "feed", "move", "sleep", "move", "vigilance", "move", "vigilance", "aggressive", "move", "vigilance")
fake_duration5.2 <- c(10, 20, 7, 10, 5, 140, 30, 25, 250, 30, 30, 60, 15, 10, 200, 23, 78, 
                    20, 45, 90, 20, 10, 45, 45, 30, 10, 23, 12, 7, 11, 24, 230, 34, 63, 14, 210, 22, 13, 54) 
fake_category5.2 <- c(rep("jalan", 15), rep("hutan", 11), rep("jalan", 13))
fake_focal5.2 <- c(rep("1", 15), rep("2", 11), rep("3", 13))

fake_data5.2 <- data.frame(fake_duration5.2, fake_behaviors5.2, fake_category5.2, fake_focal5.2)
colnames(fake_data5.2) <- c("TimeSpent", "Behavior", "Habitat", "FocalID")

X_test5.2 <- matrix(c(1, 1, 1, 1, 0, 1), nrow = 3, ncol = 2)
fake_behaviorlist5 <- c("move", "sleep", "feed", "vigilance", "aggressive")


optim_cov_ll_b1(data = fake_data5.2, X = X_test5.2, behavior_list = fake_behaviorlist5, 
                beta_starting_behavior = "move", num_cov = 2)


## The function is currently defined as
function (data, X, behavior_list, beta_starting_behavior, num_cov, 
    maxit = 5000) 
{
    num_cov <- as.numeric(num_cov)
    B <- as.numeric(length(behavior_list))
    out <- matrix(NA, nrow = num_cov, ncol = B)
    starting_point <- matrix(num_cov * rnorm(B), nrow = num_cov, 
        ncol = B)
    fit <- optim(par = starting_point, fn = function(par, data) {
        beta_start <- matrix(par, nrow = num_cov, ncol = B)
        get_ll_cov_b1(data = data, beta = beta_start, beta_starting_behavior = beta_starting_behavior, 
            behavior_list = behavior_list, X = X)
    }, data = data, control = c(fnscale = -1, maxit = maxit))
    out <- matrix(fit$par, nrow = num_cov, ncol = B)
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
