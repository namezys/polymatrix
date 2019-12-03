\name{triang_Euclidean}
\alias{triang_Euclidean}
\title{Triangularization of a polynomial matrix by Euclidean division method}

\description{
  The function \code{triang_Euclidean} triangularize the given polynomial matrix.
}

\usage{
  triang_Euclidean(pm)
}

\arguments{
  \item{pm}{polynomial matrix to triangularize}
}

\details{
The method use the for polynomilas extended Euclidean algorithm.

This method search a solution of the triangulrization by the method of Sylvester matrix,
descripted in the article Labhalla-Lombardi-Marlin (1996).
}

\references{
  Thomas Kailaith:Linear Systems, Prentice-Hall, 1980, pp 373-376
}

\author{Nikolai Ryzhkov, \email{namezys@gmail.com}}



\seealso{
  \code{\link{triang_Sylvester}}
}

\keyword{triangularization}
