\name{degree.polyMatrix}
\alias{degree.polyMatrix}
\alias{degree.polynomial}

\title{The degree of a polynomial or a polynomial matrix}

\description{The degree of the highest non-zero coefficient of the given polynomials.}

\usage{
  \method{degree}{polynomial}(p, \dots)
  \method{degree}{polyMatrix}(p,method = c("default", "matrix", "column", "row"),review=FALSE,level=2,\dots) 
}

\arguments{
  \item{p}{a \code{polynomial} or \code{polyMatrix} class object}
  \item{method}{in the case of \code{polyMatrix} the calculating method:
        \code{default}, \code{matrix}, \code{column} or \code{row}}
  \item{review}{whether we will check the degrees or not}
  \item{level}{the actions level when check the degree}
  \item{\dots}{~~~~} 
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


\seealso{\code{\link{lead}}}

\keyword{internal}

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

rm(p,pM)
}

