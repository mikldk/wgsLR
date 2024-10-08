% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate.R
\name{estimate_w_se}
\alias{estimate_w_se}
\title{Estimate standard error of error probability, w}
\usage{
estimate_w_se(x, w, method.args = list())
}
\arguments{
\item{w}{estimated error probability w}

\item{method.args}{see \code{\link[numDeriv:grad]{numDeriv::grad()}}}
}
\description{
Note that this relies on the Hessian approximation returned by
\code{\link[numDeriv:hessian]{numDeriv::hessian()}}.
}
\examples{
x <- matrix(c(1000, 10, 2, 12, 100, 8, 1, 7, 200), nrow = 3)
x
w <- estimate_w(x)
w
estimate_w_se(x, w)

}
