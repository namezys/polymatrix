\name{pMdiag}
\alias{pMdiag}
\title{Generate a diagonal polynomial square matrix}
\description{The genareted matrix is diagonal of given dimension and by given diagonal. }
\usage{
pMdiag(p,k,symb="x")
}

\arguments{
  \item{p}{a \code{polynomial} object or list of \code{polynomial} objects for diagonal elements}
  \item{k}{the row and column size of the created diagonal matrix}
  \item{symb}{the symbol used in the polynomial matrix}
}

\value{A 'polyMatrix' class polynomial matrix with given diagonal elements.}

\seealso{\code{\link{diag}}}

\examples{
pMdiag(ch2pn("1"),3)
pMdiag(ch2pn("1+2*x+3*x^2"),3)
pMdiag(ch2pn(c("x","2*x","3*x","4*x")),3)
pMdiag(ch2pn(c("x","2*x","3*x","4*x")),6)

}
\keyword{datagen}

