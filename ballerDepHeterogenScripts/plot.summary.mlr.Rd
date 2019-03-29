\name{plot.summary.mlr}
\alias{plot.summary.mlr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plotting diagnostic and calibration objects resulting from call to \code{summary.mlr}
}
\description{
Diagnostic and calibration plots, inlcuding relative squared bias reduction, constrained bias estimation, bias-variance trade-off, and power analysis.
}
\usage{
\method{plot}{summary.mlr}(x, which = 1
  , smd.index = 1:min(10, ncol(x$smd))
  , bias.index = 1:min(10, ncol(x$bias.terms))
  , orsq.plot = c(0.01, 0.05, 0.25)
  , caption.vec = c("relative squared bias reduction", "normalized bias"
    , "standardized mean difference", "maximum bias"
    , "error components", "optimum choice", "power analysis")
  , ...)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{An object of class \code{summary.mlr}, typically the result of a call to \code{summary.mlr}.}
  \item{which}{Selection of which plots to generate: \code{1} = relative squared bias reduction (by term) for a single idx, \code{2} = bias terms vs. idx, \code{3} = standardized mean difference by term vs. idx, \code{4} = maximum bias (single-covariate, subspace, absolute) vs. idx, \code{5} = bias/variance/MSE plots, \code{6} = optimum index vs. omitted r-squared, \code{7} = power analysis (matched and random subsamples) vs. idx.}
  \item{smd.index}{Index of columns in \code{smd.mat} field of \code{x} to plot.}
  \item{bias.index}{Index of columns in \code{bias.terms} field of \code{x} to plot.}
  \item{orsq.plot}{Which values for omitted R-squared to generate plots for.}
  \item{caption.vec}{Character vector to be used as caption for plots. Values will be repeated if necessary if length is shorter than number of plots requested.}
  \item{...}{Parameters to be passed to/from other functions.}
}

\details{
Currently, 7 types of plots can be generated, as specified by the \code{which} flag: 1) relative squared bias reduction, by candidate omitted term, comparing before and after matching, 2) normalized squared bias, by candidate omitted term, vs. calibration index, 3) standardized mean difference, for all included and (candidate) omitted terms, vs. calibration index, 4) aggregate bias (single-covariate maximum, covariate-subspace maximum, and absolute maximum) vs. calibration index, 5) bias/variance/MSE vs. calibration index, at user-supplied values for omitted R-squared, 6) optimal index vs. omitted R-squared, and 7) study power vs. calibration index.
}

%\value{
%}

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
