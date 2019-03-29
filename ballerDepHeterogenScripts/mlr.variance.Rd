\name{mlr.variance}
\alias{mlr.variance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Treatment effect variance
}
\description{
Calculating treatment effect variance, resulting from linear regression.
}
\usage{
mlr.variance(tr, Z.i = NULL, sigsq = 1, details = FALSE
  , idx =1:length(tr))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tr}{Binary treatment indicator vector (1=treatment, 0=control), whose coefficient in the linear regression model is TE.}
  \item{Z.i}{Matrix of adjustment covariates included in linear regression. We must have \code{nrow(Z.i) == length(tr)}.}
  \item{sigsq}{Variance of data generation noise.}
  \item{details}{Boolean flag, indicating whether intermediate objects used in generating the constrcutor vector must be returned or not (only when no repeated observations).}
  \item{idx}{Index of observations to be used, with possible duplication, e.g. as indexes of matched subset.}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
A scalar value is returned for TE variance. If \code{details = TRUE} and \code{Z.i} is not \code{NULL}, then the following objects are attached as attributes:
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

data(lalonde)

tr <- lalonde$treat
Z.i <- as.matrix(lalonde[, c("age", "educ", "black"
  , "hispan", "married", "nodegree", "re74", "re75")])

ret <- mlr.variance(tr = tr, Z.i = Z.i)

# comparing with brute-force approach
X.i <- cbind(tr, 1, Z.i)
ret2 <- (solve(t(X.i) \%*\% X.i))[1, 1]

cat("check 1:", all.equal(ret2, ret), "\n")

# matching with/without replacement
idx <- mlr.match(tr = tr, X = Z.i, caliper = 1.0
  , replace = FALSE)
idx.wr <- mlr.match(tr = tr, X = Z.i, caliper = 1.0
  , replace = TRUE)

ret3 <- mlr.variance(tr = tr, Z.i = Z.i, idx = idx)
cat("variance - matching without replacement:"
  , ret3, "\n")

ret4 <- mlr.variance(tr = tr, Z.i = Z.i, idx = idx.wr)
cat("variance - matching with replacement:"
  , ret4, "\n")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
