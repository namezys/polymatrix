% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_polyMatrix.R
\name{polyMatrix}
\alias{polyMatrix}
\title{Create polyMatrix object}
\usage{
polyMatrix(data, nrow, ncol, degree)
}
\arguments{
\item{data}{an matrix in case of creation from coefficient matrices or an numer/polynomial}

\item{nrow}{A numer of rows of matrix.
If data is a matrix, default value is the number of rows of data matrix.
In other case, it's a required parameter}

\item{ncol}{Must be positibe.
If data is a matrix, default value is the number of columns of data matrix.
In other ccase, it's a required parameter.}

\item{degree}{Degree of polynomials in coefficient matrix. Can't be negative.
If data is polynomail, degree can be evaluated automatcal.
In other case, default value is 0.}
}
\value{
new polynomial matrix of polyMatrix class
}
\description{
This function will create polynomial object fromm
coefficient matrix or signle value
}
\details{
A coefficient matrix is a matrix which contains
matrices of coefficients from lower degree to higher
side-by-side
}
