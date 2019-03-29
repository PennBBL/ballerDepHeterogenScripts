\name{mlr.power}
\alias{mlr.power}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Power analysis for causal inference using linear regression
}
\description{
Monte Carlo based calculation of study power for treatment effect estimation using linear regression on treatment indicator and adjustment covariates.
}
\usage{
mlr.power(tr, Z.i = NULL, d, sig.level = 0.05, niter = 1000
  , verbose = FALSE, idx = 1:length(tr), rnd = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tr}{Binary treatment indicator vector (1=treatment, 0=control), whose coefficient in the linear regression model is TE.}
  \item{Z.i}{Matrix of adjustment covariates included in linear regression. We must have \code{nrow(Z.i) == length(tr)}.}
  \item{d}{Standardized effect size, equal to treatment effect divided by standard deviation of generative noise.}
  \item{sig.level}{Significance level for rejecting null hypothesis.}
  \item{niter}{Number of Monte Carlo simulations used for calculating power.}
  \item{verbose}{If \code{TRUE}, calculated power is printed.}
  \item{idx}{Subset of observations to use for power calculation.}
  \item{rnd}{Boolean flag. If \code{TRUE}, power is also calculated for random subsampling of observations, using same treatment and control group sizes as indicated by \code{idx}.}
}

\details{
In each Monte Carlo iteration, response variable is generated from a normal distribution whose mean is equal to \code{d * tr} (other coefficients are assumed to be zero since their value does not affect power calculation), and whose standard deviation is \code{1.0}. Then OLS-based regression is performed on data, and p-value for treatment effect is compared to \code{sig.level}, based on which null hypothesis (no effect) is rejected or accepted. The fraction of iterations where null hypothesis is rejected is taken to be power. Standard error is calculated using a binomial-distribution assumption.
}

\value{
A numeric vector is returned. If \code{rnd} is \code{FALSE}, meand and standard error of calculated power is returned. If \code{rnd} is \code{TRUE}, mean and standard error of power calculated for random subsampling of observations is returned as well.
}

%\references{
%% ~put references to the literature/web site here ~
%}

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
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
