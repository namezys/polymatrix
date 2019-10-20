\name{ssetNext}
\alias{ssetNext}
\title{The next subset}
\description{Returns for a subset the next subset}
\usage{ssetNext(set)}
\arguments{
  \item{set}{the size of the full set, or 
           the indexes of subset for which the next we will}}
\details{When it is a natural number, then the program take it as the 
size of the full set, and returns the indicator of the first subset,
which is a zero vector of the given length. 
If it is zero-one vector, then
it read it as a subset indicator, and returns the indicator of the lexicographic next 
subset.}
\value{The next subset in lexicographic oder.}
\seealso{\code{\link{permNext}}, \code{\link{permSign}}}

\keyword{internal}

\examples{
ssetNext(4)# 0 0 0 0
ssetNext(c(1,0,1,0)) #  1 0 1 1 
ssetNext(c(0,1,1,1)) #  1 0 0 0
ssetNext(c(1,1,1,1)) #  4
}

