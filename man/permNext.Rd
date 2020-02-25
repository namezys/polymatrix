\name{permNext}
\alias{permNext}
\title{The next permutation}
\description{Returns for a permutation the next permutation}
\usage{permNext(prm)}
\arguments{
  \item{prm}{the length of required permutation, or
             the permutation for which the next we will}}
\details{When it is a natural number, then the program take it as the
length of the required permutation, and returns the first permutation
of the given length. If it is a sequence of natural numbers then
it read it as a permutation, and returns the lexicographic next
permutation of the same sequence.}
\value{The next permutation in lexicographic oder.}
\seealso{\code{\link{permSign}},\code{\link{ssetNext}} }

\keyword{internal}

\examples{
# expect 1 2 3 4
permNext(4)
permNext(c(1,2,3,4))
permNext(c(4,2,3,1))
}

