\name{polyMconvert}
\alias{polyMconvert}
\title{Conversion between the pairs of the four polyMatrix subclasses}

\description{
  The \code{polyMatrix} package has 4 different but equivalent methods to store
  the polynomial matrices.

  This function provides a conversion between the 4 methods.
  In the background operates one of the existing 12 subrutines,
  corresponding to the sub-class of the given input object
  and the required sub-class of the output object.
}

\usage{polyMconvert(pm, newclass)}

\arguments{
  \item{pm}{an arbitrary \code{polyMatrix} class object}
  \item{newclass}{
    a class identifier character string, one of the following: \\
    \code{"polyMarray"}, \code{"polyMbroad"}, \code{"polyMcells"} or \code{"polyMdlist"}
  }
}

\value{
  A \code{polyMatrix} class object, equivalent with the \code{pM} input object,
  but stored in the new, given format by the \code{newclass} parameter.
}

\seealso{\code{\link{polyMatrix-package}}}

\keyword{utilities}

\examples{
set.seed(2)
pa <- polyMgen.a(rand=function(x) rpois(x,1)) # Poisson(1) distributed coefficients
class(pa) # "polyMarray" "polyMatrix"
str(pa)
pa

pb <- polyMconvert(pa,'polyMbroad')
class(pb) # "polyMbroad" "polyMatrix"
str(pb)
pb

pc <- polyMconvert(pa,'polyMcells')
class(pc) # "polyMcells" "polyMatrix"
str(pc)
pc

pd <- polyMconvert(pa,'polyMcells')
class(pd) # "polyMdlist" "polyMatrix"
str(pd)
pd

px <- polyMconvert(pa,'polyMarray')
class(px) # "polyMarray" "polyMatrix"
str(px)
px

# clean up
# rm(pa, pb, pc, pd, px)
}
