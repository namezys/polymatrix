\name{constConv}
\alias{M2pM}
\alias{pM2M}
\title{Conversion of constant matrices}

\description{
  Conversion between the 'matrix' and 'polyMatrix' class representation of constant matrices.
}

\usage{
  M2pM(m, class=CLASS_MARRAY)
  pM2M(pM)
}

\arguments{
  \item{m}{a matrix class object}
  \item{class}{the required class of the value: \code{CLASS_MARRAY}, \code{CLASS_MBROAD},
               \code{CLASS_MCELLS}, and \code{CLASS_MDLIST}}
  \item{pM}{a polyMatrix class object}
}

\details{
  \code{M2pM} converts the \code{matrix} class objets to an \code{polyMatrix} class object.
  \code{pM2M} converts the zero degree \code{polyMatrix} class objects  to an \code{matrix} object.
}

\value{
  The \code{M2pM(m)} returns the given '\code{m}' matrix in '\code{polyMatrix}' class form.
  The \code{pM2M(pM)} returns the given '\code{pM}' polynomial matrix in '\code{matrix}' class form,
  if it is a constant matrix, otherwise gives an error message.
}

\seealso{
  \code{\link{coefs}},
  \code{\link{lead}},
  \code{\link{degree}}
}

\keyword{utilities}

\examples{
m <- matrix(1:12, 3,4)
m
pM <- M2pM(m)
class(pM)
pM

M2pM(m, "polyMarray")
M2pM(m, "polyMbroad")
M2pM(m, "polyMcells")
M2pM(m, "polyMdlist")

# clean up
# rm(m, pM)

}

