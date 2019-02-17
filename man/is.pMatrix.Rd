\name{is.pMatrix}
\alias{is.pMatrix}
\title{Is an object of Type pMatrix?}
\description{Checks wether the given object is a \code{pMatrix} object or not.}
\usage{is.pMatrix(x) }
\arguments{
  \item{x}{an R object.}   }
\details{is.pMatrix returns TRUE if its argument is a \code{pMatrix}
(that is, has "pMatrix" amongst its classes
and one of sub-classes "pMarray", "pMbroad", "pMcells" or "pMdlist") and FALSE otherwise.}

\keyword{check}

\seealso{\code{\link{pMstr}}}

\examples{
pm <- pMgen.a(2,3,5)

is.pMatrix(pMconvert(pm,"pMarray"))
is.pMatrix(pMconvert(pm,"pMdlist"))
is.pMatrix(pMconvert(pm,"pMbroad"))
is.pMatrix(pMconvert(pm,"pMdlist"))

class(pm)<-"pMatrix"
is.pMatrix(pMconvert(pm,"pMdlist")) # FALSE
class(pm)<-"pMarray"
is.pMatrix(pMconvert(pm,"pMarray")) # FALSE

class(pm)<-c("pMarray","pMatrix")
is.pMatrix(pm)

rm(pm)

} 


