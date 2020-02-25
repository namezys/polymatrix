\name{polyMul}
\alias{polyMul}
\title{Multiplication of two polynomial by matrix multiplication}
\description{ ... }
\usage{polyMul(p,q)}
\arguments{
  \item{p}{... \code{polyMgen}}
  \item{q}{... \code{polyMgen}}
}
\details{more details than the description above }
\value{Describe the value returned!! If it is a LIST, use \item{comp1 }{Description of 'comp1'}}
\note{further notes }
\author{who you are }
\references{references to the literature }
\seealso{objects to See Also as \code{\link{polyMatrix-package}}}
\keyword{math}
\examples{
set.seed(12345)
p <- polynom::polynomial(rpois(rgeom(1,.2)+1,2))
q <- polynom::polynomial(rpois(rgeom(1,.2)+1,2))
p
q
p * q
as.numeric(p * q) # 9 30 30 18 19 14  7  3
polyMul(p, q)
polyMul(q, p)

# clean up
# rm(p, q)

}

