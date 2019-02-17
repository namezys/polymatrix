\name{colMin}
\alias{colMin}
\title{Column minimums for numeric matrices}
\description{ ... }
\usage{
colMin(m)
}

\arguments{
  \item{m}{a matrix class object}
}

\details{more details than the description above }

\value{A vector by the column or row maximums of the given matrix.}

\seealso{\code{\link{colMax}},\code{\link{rowMax}},\code{\link{rowMin}}}

\keyword{utilities}

\examples{
  set.seed(123)
  M <- matrix(rpois(12,3),3,4)
  M
  # 2 5 3 3
  # 4 6 5 6
  # 2 0 3 3

  colMin(M)  # c(4,6,5,6)
  rm(M)
}

