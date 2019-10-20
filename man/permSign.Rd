\name{permSign}
\alias{permSign}
\title{The sign of the permutation}
\description{The sign of a permutation equals to the parity of the count of involutions to order the given permutation }
\usage{permSign(prm)}
\arguments{
  \item{prm}{The permutation to evaluate}   }
\value{The parity of the given permutation. Equals to +1 if even and -1 if it is odd.}
\seealso{\code{\link{permNext}}, \code{\link{ssetNext}} }

\keyword{internal}

\examples{
permSign(c(1,2,3,4))
permSign(c(1,2,4,3))
permSign(c(2,1,4,3))
permSign(c(2,4,1,3))
}

