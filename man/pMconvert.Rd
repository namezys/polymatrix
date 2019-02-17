\name{pMconvert}
\alias{pMconvert}
\title{Conversion between the pairs of the four pMatrix subclasses}

\description{The \code{pMatrix} package has 4 different but equivalent method to store
the polynomial matrices. This function provide a conversion between the 4 methods. In the
background operate one of 12 method, matching to the sub-class of the given input object 
and the required sub-class of the output object.}

\usage{pMconvert(pM,newclass)}

\arguments{
  \item{pM}{an arbitrary \code{pMatrix} class object}   
  \item{newclass}{an class identifier character string, one of 
        \code{"pMarray"}, \code{"pMbroad"}, \code{"pMcells"} or \code{"pMdlist"}}   
		}

\value{An \code{pMatrix} class object, equivalent with the \code{pM} input object, 
       but stored in the new, given format by the \code{newclass} parameter.}

\seealso{\code{\link{pMatrix-package}}}

\keyword{utilities}

\examples{
set.seed(2)
pa<-pMgen.a(rand=function(x) rpois(x,1)) # Poisson(1) distributed coefficients
class(pa) # "pMarray" "pMatrix"
str(pa)
pa

pb<-pMconvert(pa,'pMbroad')
class(pb) # "pMbroad" "pMatrix"
str(pb)
pb

pc<-pMconvert(pa,'pMcells')
class(pc) # "pMcells" "pMatrix"
str(pc)
pc

pd<-pMconvert(pa,'pMcells')
class(pd) # "pMdlist" "pMatrix"
str(pd)
pd

px<-pMconvert(pa,'pMarray')
class(px) # "pMarray" "pMatrix"
str(px)
px

rm(pa,pb,pc,pd,px,.Random.seed)

}

