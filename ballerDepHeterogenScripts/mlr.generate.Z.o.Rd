\name{mlr.generate.Z.o}
\alias{mlr.generate.Z.o}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generating omitted covariates from included covariates
}
\description{
Utility function for generating interaction terms and step functions from a set of base covariates, to be used as candidate omitted covariates.
}
\usage{
mlr.generate.Z.o(X, interaction.order = 3, step.funcs = TRUE
  , step.thresh = 20, step.ncuts = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{Matrix of base covariates.}
  \item{interaction.order}{Order of interactions to generate. It must be at least 2.}
  \item{step.funcs}{Boolean flag, indicating whether (binary) step functions must be generated from continuous variables.}
  \item{step.thresh}{Minimum number of distinct values in a numeric vector to generate step functions from.}
  \item{step.ncuts}{How many cuts to apply for generating step functions.}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
TBD
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
