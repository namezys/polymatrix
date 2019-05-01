\name{pMsgn}
\alias{pMsgn}
\title{Change the sign of a polynomial matrix object}
\description{ ... }
\usage{pMsgn(pm)}
\arguments{
  \item{pm}{a polynomial matrix object}          }
\details{For polynomial matrices returns the -1 times of the given value}
\value{The polynomial matrix for which the pm+pMsgn(pm) equals a zero polynomial matrix}
\seealso{\code{\link{Ops.polyMatrix}}}
\examples{

A<-polyMgen.a()
A
class(A) # "polyMarray" "polyMatrix"

A - polyMconvert(A,"polyMbroad")
A - polyMconvert(A,"polyMcells")
A - polyMconvert(A,"polyMdlist")

A + pMsgn(polyMconvert(A,"polyMbroad"))
A + pMsgn(polyMconvert(A,"polyMcells"))
A + pMsgn(polyMconvert(A,"polyMdlist"))

rm(A)
}
\keyword{manip}

