% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/method_inv.R
\name{inv}
\alias{inv}
\title{Inverse polynomial matrix}
\usage{
inv(x, eps = ZERO_EPS)
}
\arguments{
\item{x}{an polynomial matrix}

\item{eps}{zero threshold}
}
\description{
During inversion we will try to round to zero
}
\details{
Right now only matrices with numerical determinant is supported
}
