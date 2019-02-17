\name{pMsgn}
\alias{pMsgn}
\title{Change the sign of a polynomial matrix object}
\description{ ... }
\usage{pMsgn(pm)}
\arguments{
  \item{pm}{a polynomial matrix object}          }
\details{For polynomial matrices returns the -1 times of the given value}
\value{The polynomial matrix for which the pm+pMsgn(pm) equals a zero polynomial matrix}
\seealso{\code{\link{Ops.pMatrix}}}
\examples{

A<-pMgen.a()
A
class(A) # "pMarray" "pMatrix"

A - pMconvert(A,"pMbroad")
A - pMconvert(A,"pMcells")
A - pMconvert(A,"pMdlist")

A + pMsgn(pMconvert(A,"pMbroad"))
A + pMsgn(pMconvert(A,"pMcells"))
A + pMsgn(pMconvert(A,"pMdlist"))

rm(A)
}
\keyword{manip}

