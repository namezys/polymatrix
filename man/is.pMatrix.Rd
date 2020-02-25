\name{is.polyMatrix}
\alias{is.polyMatrix}
\title{Is an object of type '\code{polyMatrix}'?}
\description{Checks wether the given object is a \code{polyMatrix} object or not.}
\usage{is.polyMatrix(x)}
\arguments{
  \item{x}{an R object.}
}
\details{
  \code{is.polyMatrix} returns \code{TRUE} if its argument is a \code{polyMatrix}
  (that is, has "polyMatrix" amongst its classes and one of the four sub-classes:
  "polyMarray", "polyMbroad", "polyMcells", "polyMdlist") and \code{FALSE} otherwise.
}

\keyword{check}

\seealso{\code{\link{pMstr}}}

\examples{
pm <- polyMgen.a(2,3,5)

is.polyMatrix(polyMconvert(pm,"polyMarray"))
is.polyMatrix(polyMconvert(pm,"polyMdlist"))
is.polyMatrix(polyMconvert(pm,"polyMbroad"))
is.polyMatrix(polyMconvert(pm,"polyMdlist"))

# clean up
# rm(pm)
}
