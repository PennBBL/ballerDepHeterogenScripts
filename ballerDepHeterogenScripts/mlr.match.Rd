\name{mlr.match}
\alias{mlr.match}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Thin wrapper around \code{Match} function from \code{Matching} package
}
\description{
Performs propensity score or Mahalanobis matching and return indexes of treatment and control groups.
}
\usage{
mlr.match(tr, X, psm = TRUE, replace = F, caliper = Inf
  , verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tr}{Binary treatment indicator vector (1=treatment, 0=control), whose coefficient in the linear regression model is TE.}
  \item{X}{Covariates used in matching, either directly (Mahalanobis matching) or indirectly (propensity score).}
  \item{psm}{Boolean flag, indicating whether propensity score matching should be used (\code{TRUE}) or Mahalanobis matching (\code{FALSE}).}
  \item{replace}{Boolean flag, indicating whether matching must be done with or without replacement.}
  \item{caliper}{Size of caliper (standardized distance of two observations) used in matching. Treatment and control observations with standardized distance larger than \code{caliper} will not be considered as eligible pairs duing matching.}
  \item{verbose}{Boolean flag, indicating whether size of treatment and control groups before and after matching will be printed.}
  \item{\dots}{Other arguments to be passed to \code{Match} function.}
}

\details{
For propensity score matching, linear predictors from logistic regression are used (rather than predicted probabilities).
}

\value{
A vector of matched indexes, containing both treatment and control groups. Also, the following attributes are attached: 1) \code{nt}: size of treatment group, 2) \code{nc}: size of control group, 3) \code{psm.reg}: logistic regression object used in generating propensity scores (\code{NA} if \code{psm} is \code{FALSE}), 4) \code{match.obj}: matching object returned by \code{Match} function.
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

\examples{

data(lalonde)

tr <- lalonde$treat
Z.i <- as.matrix(lalonde[, c("age", "educ", "black"
  , "hispan", "married", "nodegree", "re74", "re75")])
Z.o <- model.matrix(~ I(age^2) + I(educ^2) + I(re74^2) + I(re75^2) - 1, lalonde)

# propensity score matching on all covariates
idx <- mlr.match(tr = tr, X = cbind(Z.i, Z.o), caliper = 1.0, replace = FALSE)

# improvement in maximum single-covariate bias due to matching
bias.obj.before <- mlr.bias(tr = tr, Z.i = Z.i, Z.o = Z.o)
bias.before <- bias.obj.before$subspace$bias
dir <- bias.obj.before$subspace$dir
bias.after <- as.numeric(mlr.bias(tr = tr[idx]
  , Z.i = Z.i[idx, ], Z.o = dir[idx], gamma.o = 1.0)$single$bias)

# percentage bias-squared rediction
cat("normalized bias - before:", bias.before, "\n")
cat("normalized bias - after:", bias.after, "\n")
cat("percentage squared-bias reduction:"
  , (bias.before^2 - bias.after^2)/bias.before^2, "\n")

# matching with replacement
idx.wr <- mlr.match(tr = tr, X = cbind(Z.i, Z.o), caliper = 1.0
  , replace = TRUE)
bias.after.wr <- as.numeric(mlr.bias(tr = tr
  , Z.i = Z.i, Z.o = dir, gamma.o = 1.0, idx = idx.wr)$single$bias)
cat("normalized bias - after (with replacement):", bias.after.wr, "\n")

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
