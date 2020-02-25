\name{matrixMinMax}
\alias{colMax}
\alias{colMin}
\alias{rowMax}
\alias{rowMin}
\title{Maximums and minimums for numeric matrices}
\description{Calculate the maximums or minimums column-wise or row-wise, depending on the called method.}

\usage{
  colMax(matrix) # column maximums
  colMin(matrix) # column minimums
  rowMax(matrix) # row maximums
  rowMin(matrix) # row minimums
}
\arguments{
  \item{matrix}{a \code{matrix} class object}
}

\value{A vector by the column or row maximums or minimums of the given matrix.}

\seealso{
  \code{\link{colMin}},
  \code{\link{rowMax}},
  \code{\link{rowMin}}
}

\keyword{utilities}

\examples{
  set.seed(123)
  M <- matrix(rpois(12,3),3,4)
  M
  # 2 5 3 3
  # 4 6 5 6
  # 2 0 3 3

  colMax(M)  # c(4,6,5,6)
  colMin(M)  # c(2,0,3,3)
  rowMax(M)  # c(5,6,3)
  rowMin(M)  # c(2,4,0)
  # clean up
  # rm(M)
}
