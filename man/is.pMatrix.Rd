\name{is.polyMatrix}
\alias{is.polyMatrix}
\title{Is an object of Type polyMatrix?}
\description{Checks wether the given object is a \code{polyMatrix} object or not.}
\usage{is.polyMatrix(x) }
\arguments{
  \item{x}{an R object.}   }
\details{is.polyMatrix returns TRUE if its argument is a \code{polyMatrix}
(that is, has "polyMatrix" amongst its classes
and one of sub-classes "polyMarray", "polyMbroad", "polyMcells" or "polyMdlist") and FALSE otherwise.}

\keyword{check}

\seealso{\code{\link{pMstr}}}

\examples{
pm <- polyMgen.a(2,3,5)

is.polyMatrix(polyMconvert(pm,"polyMarray"))
is.polyMatrix(polyMconvert(pm,"polyMdlist"))
is.polyMatrix(polyMconvert(pm,"polyMbroad"))
is.polyMatrix(polyMconvert(pm,"polyMdlist"))

class(pm)<-"polyMatrix"
is.polyMatrix(polyMconvert(pm,"polyMdlist")) # FALSE
class(pm)<-"polyMarray"
is.polyMatrix(polyMconvert(pm,"polyMarray")) # FALSE

class(pm)<-c("polyMarray","polyMatrix")
is.polyMatrix(pm)

rm(pm)

} 


