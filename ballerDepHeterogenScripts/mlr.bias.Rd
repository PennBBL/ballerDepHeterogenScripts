\name{mlr.bias}
\alias{mlr.bias}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Treatment effect bias
}
\description{
Calculating treatment effect bias due to misspecified regression, using coefficients of omitted covariates (if supplied) or a constrained bias estimation approach.
}
\usage{
mlr.bias(tr, Z.i = NULL, Z.o, gamma.o = NULL
  , idx = 1:length(tr))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tr}{Binary treatment indicator vector (1=treatment, 0=control), whose coefficient in the linear regression model is TE.}
  \item{Z.i}{Matrix of adjustment covariates included in linear regression. We must have \code{nrow(Z.i) == length(tr)}.}
  \item{Z.o}{Matrix of adjustment covariates (present in generative model but) omitted from regression estimation. We must have \code{nrow(Z.o) == length(tr)}.}
  \item{gamma.o}{Vector of coefficients for omitted adjustment covariates.}
  \item{idx}{Index of observations to be used, with possible duplication, e.g. as indexes of matched subset.}
}

\details{
For \code{single}, \code{subspace} and \code{absolute}, biases are calculated using the constrained bias estimation framework, i.e. L2 norm of \code{Z.o\%*\%gamma.o} is taken to be \code{length(tr)} (mean squared of 1).
}

\value{
A list with the following elements is returned:
  \item{gamma.o}{If function argument \code{gamma.o} is \code{NULL}, this field will be \code{NA}. Otherwise, this will be the covariate omission bias for the given coefficient values.}
  \item{single}{A list with elements: 1) \code{bias}: bias for the omitted covariate with maximum absolute bias, 2) \code{bias.vec}: vector of biases for all omitted covariates, 3) \code{dir}: vector of length \code{length(tr)}, being the particular column of \code{Z.o} with maximum absolute bias (after orthogonalization and normalization), 4) \code{idx}: column number for \code{Z.o} corresponding to \code{dir}.}
  \item{subspace}{A list with elements: 1) \code{bias}: bias in direction within omitted covariate subspace with maximum absolute bias, 2) \code{dir}: direction in omitted-covariate subspace (and orthogonal to subspace spanned by \code{{1,Z.i}}) corresponding to the bias in previous element.}
  \item{absolute}{A list with elements: 1) \code{bias}: bias in direction within subspace orthogonal to \code{{1,Z.i}} with maximum absolute bias, 2) \code{dir}: direction in aforementioned subspace corresponding to maximum absolute bias.}
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
# number of omitted covariates
Ko <- 3

# treatment indicator variable
tr <- c(rep(1, Nt), rep(0, Nc))
# matrix of included (adjustment) covariates
Z.i <- matrix(runif(K*N), ncol = K)
# matrix of omitted covariates
Z.o <- matrix(runif(Ko*N), ncol = Ko)
# coefficients of omitted covariates
gamma.o <- runif(Ko)

retobj <- mlr.bias(tr = tr, Z.i = Z.i, Z.o = Z.o, gamma.o = gamma.o)

# 1) using actual coefficients for computing bias
ret <- retobj$gamma.o

# comparing with brute-force approach
X.i <- cbind(tr, 1, Z.i)
ret2 <- (solve(t(X.i) \%*\% X.i, t(X.i) \%*\% Z.o \%*\% gamma.o))[1]

cat("check 1:", all.equal(ret2, ret), "\n")

# comparing with single method
Z.o.proj <- mlr.orthogonalize(X = cbind(1, Z.i), Z = Z.o, normalize = TRUE)
ret3 <- (solve(t(X.i) \%*\% X.i, t(X.i) \%*\% Z.o.proj))[1, ]

cat("check 2:", all.equal(ret3, retobj$single$bias.vec), "\n")

ret4 <- (solve(t(X.i) \%*\% X.i, t(X.i) \%*\% retobj$subspace$dir))[1, ]

cat("check 3:", all.equal(as.numeric(ret4), as.numeric(retobj$subspace$bias)), "\n")

ret4 <- (solve(t(X.i) \%*\% X.i, t(X.i) \%*\% retobj$absolute$dir))[1, ]

cat("check 4:", all.equal(as.numeric(ret4), as.numeric(retobj$absolute$bias)), "\n")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
