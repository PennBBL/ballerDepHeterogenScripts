\name{mlr.bias.constructor}
\alias{mlr.bias.constructor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generating the treatment effect bias constructor vector
}
\description{
Generaring the vector that, multiplied by \code{Z.o\%*\%gamma.o} (contribution of omitted covariates to outcome), produces the treatment effect bias - due to model misspecification in the form of covariate omission - when using linear regression for causal inference.
}
\usage{
mlr.bias.constructor(tr, Z.i = NULL, details = FALSE, idx = 1:length(tr))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tr}{Binary treatment indicator vector (1=treatment, 0=control), whose coefficient in the linear regression model is TE.}
  \item{Z.i}{Matrix of adjustment covariates included in linear regression. We must have \code{nrow(Z.i) == length(tr)}.}
  \item{details}{Boolean flag, indicating whether intermediate objects used in generating the constrcutor vector must be returned or not. This only works if at least one adjustment covariate is included in the regression (\code{Z.i} is not \code{NULL}), and there are no repeated observations, i.e. \code{max(table(idx))==1}.}
  \item{idx}{Index of observations to be used, with possible duplication, e.g. as indexes of matched subset.}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
A vector of same length as \code{tr} is returned. If \code{details = TRUE} and \code{Z.i} is not \code{NULL}, then the following objects are attached as attributes:
  \item{p}{Vector of length \code{ncol(Z.i)}, reflecting the sum of each included covariate in treatment group.}
  \item{q}{Vector of length \code{ncol(Z.i)}, reflecting the sum of each included covariate across both treatment and control groups.}
  \item{u.i}{Vector of length \code{ncol(Z.i)}, reflecting the mean difference between groups (control - treatment) for each included covariate.}
  \item{A}{Weighted, within-group covariance matrix of included covariates. It is a square matrix of dimension \code{ncol(Z.i)}.}
  \item{iA}{Inverse of \code{A}.}
}

\references{
Link to a draft paper, documenting the supporting mathematical framework, will be provided in the next release.
}

\author{
Alireza S. Mahani, Mansour T.A. Sharabiani
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}

\examples{

# number of included adjustment covariates
K <- 10
# number of observations in treatment group
Nt <- 100
# number of observations in control group
Nc <- 100
N <- Nt + Nc

# treatment indicator variable
tr <- c(rep(1, Nt), rep(0, Nc))
# matrix of included (adjustment) covariates
Z.i <- matrix(runif(K*N), ncol = K)

ret <- mlr.bias.constructor(tr = tr, Z.i = Z.i)

# comparing with brute-force approach
X.i <- cbind(tr, 1, Z.i)
ret2 <- (solve(t(X.i) \%*\% X.i, t(X.i)))[1, ]

cat("check 1:", all.equal(ret2, ret), "\n")

# sampling with replacement
idx <- sample(1:N, size = round(0.75*N), replace = TRUE)
ret3 <- mlr.bias.constructor(tr = tr, Z.i = Z.i, idx = idx)
ret4 <- (solve(t(X.i[idx, ]) \%*\% X.i[idx, ], t(X.i[idx, ])))[1, ]

cat("check 2:", all.equal(ret3, ret4), "\n")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
