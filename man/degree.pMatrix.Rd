\name{degree.polyMatrix}
\alias{degree.polyMatrix}
\alias{degree.polynomial}

\title{The degree of a polynomial or a polynomial matrix}

\description{The degree of the highes non-zero coefficient of the given polynomials.}

\usage{
  \method{degree}{polynomial}(p, \dots)
  \method{degree}{polyMatrix}(p,method = c("default", "matrix", "column", "row"),\dots) 
}

\arguments{
  \item{p}{a \code{polynomial} or \code{polyMatrix} class object}
  \item{method}{in the case of \code{polyMatrix} the calculating method:
  \code{default}, \code{matrix}, \code{column} or \code{row}}
  \item{\dots}{~~Describe \code{...} here~~} 
} 


\value{
When the input is a \code{polynomial} class object, then simply a natural number,
the degree of the given polynom.
\cr

When the input is a \code{polyMatrix} class object, then 
in case of \code{method="matrix"}
a matrix by the same size as the input, containing the degrees of the polynomials,
in the given matrix over polynomials. 
In case of \code{method="column"} or \code{="row"} it is the column or row 
maximums of the polynom matrix degrees. 
In case of \code{method="default"} it is the maximum degree of polynoms in the matrix polynom.
}

\note{further notes }

\author{who you are }

\references{references to the literature }

\seealso{\code{\link{lead}}}

\keyword{internal}

\examples{
p<-polynom::polynomial(0:12)
class(p) # polynom
degree(p)

pM<-polyMgen.d(3,3,rawData=
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

rm(p,pM)
}

