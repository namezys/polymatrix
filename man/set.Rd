\name{set}
\alias{set}

\title{Overwrite a subpart of a polynomial matrix}

\description{Returns a polynomial matrix in which the given element are changed to the new given elements.}

\usage{
  \method{set}{polyMatrix}(pm, i, j, m, byrow=FALSE)
}

\arguments{
  \item{pm}{a \code{polyMatrix} class object}
  \item{i}{the vector of row}
  \item{j}{the vector of column indexes}
  \item{m}{the list of \code{polynomial} class new elements}
  \item{byrow}{logical. If FALSE (the default) the \code{m} used by columns, otherwise by rows}
}

\value{
  A polynomial matrix with the over written elements.
}


\seealso{\code{\link{lead}}}

\keyword{internal}

\examples{
rawData <- ch2pn(c("-3+x^3","2+4*x","x^2","1","2","3+x","2*x","0","2-3*x"))
pM <- polyMgen.d(3, 3, rawData=rawData)
pM

set(pM, 1:2, 1:2, ch2pn(c("3","4","x","x^2")))
pM

# clean up
# rm(pM)
}

