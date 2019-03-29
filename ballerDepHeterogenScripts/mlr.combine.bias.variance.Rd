\name{mlr.combine.bias.variance}
\alias{mlr.combine.bias.variance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Combining bias and variance to produce total MSE for treatment effect
}
\description{
Combining normalized bias and variance over a range of values for omitted R-squared to produce normalized MSE.
}
\usage{
mlr.combine.bias.variance(tr, bvmat, orsq.min = 0.001, orsq.max = 1, n.orsq = 100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tr}{Binary treatment indicator vector (1=treatment, 0=control), whose coefficient in the linear regression model is TE.}
  \item{bvmat}{Matrix of bias and variances. First column must be bias, and second column must be variance. Each row corresponds to a different `calibration index' or scenario, which we want to compare and find the best among them.}
  \item{orsq.min}{Minimum omitted R-squared used for combining bias and variance.}
  \item{orsq.max}{Maximum omitted R-squared.}
  \item{n.orsq}{Number of values for omitted R-squared generated in the vector.}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
A list with the following elements:
  \item{orsq.vec}{Vector of omitted R-squared values used for combining bias and variance.}
  \item{errmat}{Matrix of MSE, with each row corresponding to an omitted R-squared value, and each column for a value of calibration index, i.e. one row if \code{bvmat}.}
  \item{biassq.mat}{Matrix of squared biases, with a structure similar to \code{errmat}.}
  \item{which.min.vec}{Value of calibration index (row number for \code{errmat}) with minimum MSE.}
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
%\examples{
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
