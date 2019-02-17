\name{rowMax}
\alias{rowMax}
\title{Row maximums for numeric matrices}
\description{ ... }
\usage{
rowMax(m)
}

\arguments{
  \item{m}{a matrix class object}
}

\details{more details than the description above }

\value{A vector by the column or row maximums of the given matrix.}

\seealso{\code{\link{colMax}},\code{\link{colMin}},\code{\link{rowMin}}}

\keyword{internal}

\examples{
  set.seed(123)
  M <- matrix(rpois(12,3),3,4)
  M
  # 2 5 3 3
  # 4 6 5 6
  # 2 0 3 3

  rowMax(M)  # c(5,6,3)
  rm(M)
}

