\name{pMprod}
\alias{pMprod}
\title{Product of the elements of the given polynomial vector}
\description{The product of the elements of the vector \code{pm} given in
\code{pMatrix} class object format.}
\usage{pMprod(pm)}
\arguments{\item{pm}{a \code{pMatrix} class object} }
\details{The input must be a \code{pMatrix} class vector object. The result is a 
\code{polynomial} class polynom.}
\value{A \code{polynomial} which is the product of the elements of the \code{pm} argument}
\seealso{\code{\link{pVsk}}}
\keyword{math}
\examples{
set.seed(32)
A <- pMgen.a(1,2)
A
pMprod(A)
rm(A,.Random.seed)
}

