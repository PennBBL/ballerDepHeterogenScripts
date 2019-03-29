\name{mlr}
\alias{mlr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Creating a series of matched data sets with different calibration parameters
}
\description{
Creating a series of matched data sets with different calibration parameters. The output of this function can be supplied to \code{summary.mlr} and then \code{plot.summary.mlr} methods to generate diagnostic and calibration plots.
}
\usage{
mlr(tr, Z.i = NULL, Z.o = mlr.generate.Z.o(Z.i), psm = TRUE
  , caliper.vec = c(0.1, 0.25, 0.5, 0.75, 1, 1.5, 2, 5, Inf)
  , ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tr}{Binary treatment indicator vector (1=treatment, 0=control), whose coefficient in the linear regression model is TE.}
  \item{Z.i}{Matrix of adjustment covariates included in linear regression. We must have \code{nrow(Z.i) == length(tr)}.}
  \item{Z.o}{Matrix of adjustment covariates (present in generative model but) omitted from regression estimation. We must have \code{nrow(Z.o) == length(tr)}.}
  \item{psm}{Boolean flag, indicating whether propensity score matching should be used (\code{TRUE}) or Mahalanobis matching (\code{FALSE}).}
  \item{caliper.vec}{Vector of matching calipers used.}
  \item{\dots}{Other parameters passed to \code{mlr.match}.}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
A list with the following fields:
  \item{tr}{Same as input.}
  \item{Z.i}{Same as input.}
  \item{Z.o}{Same as input.}
  \item{idx.list}{List of observation indexes for each matched data set.}
  \item{caliper.vec}{Same as input.}
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
