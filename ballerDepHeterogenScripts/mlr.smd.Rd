\name{mlr.smd}
\alias{mlr.smd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Standardized mean difference
}
\description{
Calculate standardized mean difference for each column of a matrix, given a binary treatment indicator vector.
}
\usage{
mlr.smd(tr, X)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tr}{Binary treatment indicator vector; 1 means treatment, 0 means control.}
  \item{X}{Matrix of covariates; each column is a covariate whose standardized mean difference we want to calculate. \code{nrow(X)} must be equal to \code{length(tr)}.}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
A vector of length \code{ncol(X)}, containing standardized mean differences for each column of \code{X}, given treatment variable \code{tr}.
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
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
