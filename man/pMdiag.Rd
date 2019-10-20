\name{pMdiag}
\alias{pMdiag}
\title{Generates a diagonal polynomial square matrix}

\description{
The genareted matrix a diagonal matrix of the given dimension
with the diagonal elements given as input.
}

\usage{
pMdiag(p, diag_dim, symb="x")
}

\arguments{
  \item{p}{a \code{polynomial} object or list of \code{polynomial} objects for diagonal elements}
  \item{diag_dim}{the row and column size of the diagonal matrix}
  \item{symb}{the symbol used in the polynomial matrix}
}

\value{A 'polyMatrix' class type polynomial matrix with given diagonal elements.}

\seealso{\code{\link{diag}}}

\examples{
pMdiag(ch2pn("1"),3)
pMdiag(ch2pn("1+2*x+3*x^2"),3)
pMdiag(ch2pn(c("x","2*x","3*x","4*x")),3)
pMdiag(ch2pn(c("x","2*x","3*x","4*x")),6)
}

\keyword{datagen}

