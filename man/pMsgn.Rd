\name{pMsgn}
\alias{pMsgn}
\title{Change the sign of a polynomial matrix object}

\description{
Multiplies the given \code{polyMatrix} by \code{-1}, and
returns the \code{-1} times the given polynomial matrix.
}

\usage{pMsgn(pm)}

\arguments{
  \item{pm}{a polynomial matrix object}          }

\value{
The polynomial matrix for which the \code{pm+pMsgn(pm)}
equals a zero polynomial matrix.
}

\seealso{\code{\link{Ops.polyMatrix}}}

\examples{

A <- polyMgen.a()
A
class(A) # "polyMarray" "polyMatrix"

A - polyMconvert(A,"polyMbroad")
A - polyMconvert(A,"polyMcells")
A - polyMconvert(A,"polyMdlist")

A + pMsgn(polyMconvert(A,"polyMbroad"))
A + pMsgn(polyMconvert(A,"polyMcells"))
A + pMsgn(polyMconvert(A,"polyMdlist"))

# clean up
# rm(A)
}

\keyword{manip}

