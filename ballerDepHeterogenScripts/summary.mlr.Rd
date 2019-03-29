\name{summary.mlr}
\alias{summary.mlr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Applying diagnostic and calibration functions to mlr objects
}
\description{
Applying a series of diagnostic and calibration functions to a series of matched data sets to determine impact of matching on TE bias, variance and total error, and to select the best matching parameters.
}
\usage{
\method{summary}{mlr}(object, power = FALSE
  , power.control = list(rnd = TRUE, d = 0.5, sig.level = 0.05
    , niter = 1000, rnd = TRUE)
  , max.method = c("single-covariate", "covariate-subspace"
    , "absolute")
  , verbose = FALSE, ...
  , orsq.min = 1e-03, orsq.max = 1e0, n.orsq = 100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An object of class \code{mlr}, typically the result of a call to \code{mlr}.}
  \item{power}{Boolean flag indicating whether Monte-Carlo based power analysis must be performed or not.}
  \item{power.control}{A list containing parameters to be passed to \code{mlr.power} for power calculation.}
  \item{max.method}{Which constrained bias estimation method must be used in bias-variance trade-off and other analyses?}
  \item{verbose}{Whether progress message must be printed.}
  \item{...}{Parameters to be passed to/from other functions.}
  \item{orsq.min}{Minimum value of omitted R-squared used for combining normalized bias and variance.}
  \item{orsq.max}{Maximum value of omitted R-squared used for combining normalized bias and variance.}
  \item{n.orsq}{Number of values for omitted R-squared to generate in the specified range.}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
An object of class \code{summary.mlr}, with the following elements:
  \item{mlr.obj}{Same as input.}
  \item{bias}{Matrix of aggregate bias values, one row per calibration index, and three columns: 1) single-covariate maximum, 2) covariate-subspace maximum, and 3) absolute maximum, in that order.}
  \item{bias.terms}{Matrix of biases, one row per calibration index, and one column per candidate omitted term.}
  \item{variance}{Vector of normalized variances, one per each value of calibration index.}
  \item{power}{Matrix of power calculations, one row per calibration index. Each row is identical to output of \code{mlr.power} for that calibration index value.}
  \item{smd}{Matrix of standardized mean differences, one row per calibration index, and one column for each included or omitted covariates.}
  \item{combine.obj}{Output of \code{mlr.combine.bias.variance} applied to bias and variances at each calibration index value.}
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
