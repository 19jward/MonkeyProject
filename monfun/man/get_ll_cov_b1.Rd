\name{get_ll_cov_b1}
\alias{get_ll_cov_b1}
\title{
get LL given a starting behavior (w/covariates)
}
\description{
returns Log-Likelihood of data for a specific starting behavior for data w/covariates - utilizes the cov2prob functions. for use in the optim_cov_ll_full function
}
\usage{
get_ll_cov_b1(data, beta, beta_starting_behavior, behavior_list, X)
}
\arguments{
  \item{data}{
focals, note the data must have column names "FocalID", "Behavior", "TimeSpent", & "Habitat"
}
  \item{beta}{
beta is a single slice of the BxMxM array pertaining to the starting behavior (b1) (matrix)
}
  \item{beta_starting_behavior}{
b1 - behavior starting from (string)
}
  \item{behavior_list}{
list of behaviors in data
}
  \item{X}{
indicator for covariates (matrix)} 
}
\details{
}
\value{log-likelihood value}
\references{
}
\author{Josephine Ward
}
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
fake_beta_slice <- matrix(2*rnorm(5), nrow = 2, ncol = 5) 


get_ll_cov_b1(fake_data5.2, beta = fake_beta_slice, beta_starting_behavior = "move", 
              behavior_list = fake_behaviorlist5, X = X_test5.2) 


## The function is currently defined as
function (data, beta, beta_starting_behavior, behavior_list, 
    X) 
{
    num_foc <- length(unique(data$FocalID))
    B <- length(unique(data$Behavior))
    sum_LL <- numeric(0)
    data$FocalID <- as.numeric(data$FocalID)
    P <- cov2prob(X = X, beta = beta)
    for (f in 1:num_foc) {
        focal <- f
        data_focal <- subset(data, data$FocalID == f, select = c(TimeSpent, 
            Behavior, Habitat))
        P_f <- P[f, ]
        focal_LL <- get_ll_row(data = data_focal, p_row = P_f, 
            row_behavior = beta_starting_behavior, behavior_list = behavior_list)
        sum_LL <- sum(sum_LL, focal_LL)
    }
    return(sum_LL)
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
