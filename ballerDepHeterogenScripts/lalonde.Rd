\name{lalonde}
\alias{lalonde}
\docType{data}
\title{
Lalonde's National Supported Work Demonstration data
}
\description{
One of the datasets used by Dehejia and Wahba in their paper "Causal Effects in Non-Experimental Studies: Reevaluating the Evaluation of Training Programs." Also used as an example dataset in the MatchIt package.
}
\usage{data("lalonde")}
\format{
  A data frame with 614 observations on the following 10 variables.
  \describe{
    \item{\code{treat}}{treatment indicator; 1 if treated in the National Supported Work Demonstration, 0 if from the Current Population Survey}
    \item{\code{age}}{age, a numeric vector.}
    \item{\code{educ}}{years of education, a numeric vector between 0 and 18.}
    \item{\code{black}}{a binary vector, 1 if \code{black}, 0 otherwise.}
    \item{\code{hispan}}{a binary vector, 1 if \code{hispanic}, 0 otherwise.}
    \item{\code{married}}{a binary vector, 1 if \code{married}, 0 otherwise.}
    \item{\code{nodegree}}{a binary vector, 1 if no degree, 0 otherwise.}
    \item{\code{re74}}{earnings in 1974, a numeric vector.}
    \item{\code{re75}}{earnings in 1975, a numeric vector.}
    \item{\code{re78}}{earnings in 1978, a numeric vector (outcome variable).}
  }
}
\details{
This data set has been taken from \code{twang} package, with small changes to field descriptions.
}
\source{
http://www.columbia.edu/~rd247/nswdata.html http://cran.r-project.org/src/contrib/Descriptions/MatchIt.html
}
\references{
Lalonde, R. (1986). Evaluating the econometric evaluations of training programs with experimental data. American Economic Review 76: 604-620.

Dehejia, R.H. and Wahba, S. (1999). Causal Effects in Nonexperimental Studies: Re-Evaluating the Evaluation of Training Programs. Journal of the American Statistical Association 94: 1053-1062.
}

%\examples{
%}

% \keyword{datasets}
