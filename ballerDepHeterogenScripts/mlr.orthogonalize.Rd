\name{mlr.orthogonalize}
\alias{mlr.orthogonalize}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Orthogonalization of vectors with repsect to a matrix
}
\description{
Decomposing a collection of vectors into parallel and orthogonal components with respect to the subspace spanned by columns of a reference matrix.
}
\usage{
mlr.orthogonalize(X, Z, normalize = FALSE, tolerance = .Machine$double.eps^0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{Matrix whose columns form the subspace, with respect to which we want to orthogonalize columns of \code{Z}.}
  \item{Z}{Matrix whose columns we want to orthogonalize with respect to the subpsace spanned by columns of \code{X}. We must have \code{nrow(Z) == nrow(X)}.}
  \item{normalize}{Boolean flag, indicating whether the orthogonal component of \code{Z} columns must be normalized so that their L2 norms equal \code{nrow(Z)} (mean squared is 1).}
  \item{tolerance}{If unnormalized projection of a column of \code{Z} has an L2 norm below \code{tolerance}, it will not be normalized (even if requested via \code{normalize}) and instead a zero vector will be returned.}
}

\details{
Current implementation uses Singular Value Decomposition (\code{svd}) of \code{X} to form an orthonormal basis from columns of \code{X} to facilitate the projection process.
}

\value{
A matrix of same dimensions as \code{Z} is returned, with each column containing the orthogonal component of the corresponding column of \code{Z}. Parallel components are attached as \code{parallel} attribute.
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

K <- 10
N <- 100
Ko <- 5

X <- matrix(runif(N*K), ncol = K)
Z <- matrix(runif(N*Ko), ncol = Ko)

ret <- mlr.orthogonalize(X = X, Z = Z, normalize = FALSE)

orthogonal <- ret
parallel <- attr(ret, "parallel")
Z.rec <- parallel + orthogonal

# check that parallel and orthogonal components add up to Z
cat("check 1:", all.equal(as.numeric(Z.rec), as.numeric(Z)), "\n")
# check that inner product of orthogonal columns and X columns are zero
cat("check 2:", all.equal(t(orthogonal) \%*\% X, matrix(0, nrow = Ko, ncol = K)), "\n")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
