\name{degree}
\alias{degree}

\title{The degree of a polynomial or a polynomial matrix}

\description{
The degree of the highest non-zero coefficient of the polynomials.
}

\usage{degree(p, ...)}

\arguments{
  \item{p}{a \code{polynomial} or \code{polyMatrix} class object}
  \item{\dots}{supplementary options (see \code{degree.polyMatrix})}
}

\value{
When the input is a \code{polynomial} class object, then value is
the degree of the polynom.
\cr

When the input is a \code{polyMatrix} class object, then
in case of \code{method="matrix"}
the value is a matrix by the same size as the input,
containing the degrees of the polynomials.
In case of \code{method="column"} or \code{="row"}
the value are maximums of the columns or rows of the polynom matrix degrees.
In case of \code{method="default"} it is the maximum of the degree of all polynoms.
}

\seealso{\code{\link{lead}},
         \code{\link{degree.polyMatrix}},
         \code{\link{degree.polynomial}}}

\keyword{utilities}

\examples{
p <- polynom::polynomial(0:12)
class(p) # polynom
degree(p)

pM <- polyMgen.d(3,3,rawData=
   ch2pn(c("-3+x^3","2+4*x","x^2","1","2","3+x","2*x","0","2-3*x")))
pM

degree(pM) #  the maximum degree
degree(pM,"c") # column wise maximum degree
degree(pM,"r") # row wise maximum degree
degree(pM,"m") # matrix: element wise degree

degree(polyMconvert(pM,"polyMarray"),"m") # polyMarray class
degree(polyMconvert(pM,"polyMbroad"),"m") # polyMbroad class
degree(polyMconvert(pM,"polyMcells"),"m") # polyMcells class
degree(polyMconvert(pM,"polyMdlist"),"m") # polyMdlist class

# clean up
# rm(p, pM)
}

