\name{degree}
\alias{degree}

\title{The degree of a polynomial or a polynomial matrix}

\description{The degree of the highes non-zero coefficient of the given polynomials.}

\usage{degree(p, ...)}

\arguments{
           \item{p}{a \code{polynomial} or \code{polyMatrix} class object}
           \item{\dots}{supplementary options see \code{degree.polyMatrix}}
} 

\details{more details than the description above }

\value{The returned value is a single number, or a vector or a matrix, 
depending on the supplemantary parameter}

\seealso{\code{\link{lead}}, 
         \code{\link{degree.polyMatrix}}, 
         \code{\link{degree.polynomial}}}

\keyword{utilities}

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
degree(pM,"m") # matrix: elementwise degree

degree(polyMconvert(pM,"polyMarray"),"m") # polyMarray class 
degree(polyMconvert(pM,"polyMbroad"),"m") # polyMbroad class
degree(polyMconvert(pM,"polyMcells"),"m") # polyMcells class
degree(polyMconvert(pM,"polyMdlist"),"m") # polyMdlist class

rm(p,pM)
}

