\name{CommonPolynom}
\alias{GCD}
\alias{GCD.polyMatrix}
\alias{LCM}
\alias{LCM.polyMatrix}
\title{GCD and LCM for polynomial matrices}
\description{The greatest common divisor of polynomial that keeps 
             the elements of the given polynomial matrix.}
\usage{
\method{GCD}{polyMatrix}(x,type=c("total","col","row"),...)
\method{LCM}{polyMatrix}(x,type=c("total","col","row"),...)
}
\arguments{
           \item{x}{an \code{polyMatrix} class object}
           \item{type}{calculates the common polynom column-wise, row-wise or for the total matrix}
           \item{\dots}{additional arguments}                  

}
\details{
Calculates the greatest common divisor or the least common divisor of the total matrix, or row-wise
or column-wise, depending on the parameter \code{type}.}

\value{A polynom matrix which is the greatest common divisor or the least common multiple for all the elements 
      of the given polynomial matrix or for the columns or for the rows, depending on the value of 
      the '\code{type}' parameter.}

\seealso{\code{\link{pMdet}}, \code{\link{pMadj}}}

\examples{
p1 <- ch2pn("1-x")
p2 <- ch2pn("x-x^2")
p3 <- ch2pn("1-x^2")
p4 <- ch2pn("1+x")
p5 <- ch2pn("1+2*x+x^2")
p6 <- ch2pn("1-2*x+x^2")
A <- polyMgen.d(2,3,rawData=list(p1,p2,p3,p4,p5,p6))
A
# 1 - x     1 - x^2   1 + 2*x + x^2
# x - x^2   1 + x     1 - 2*x + x^2

l<-function(...) structure(list(...), class = "polylist")

GCD(A) # 1
GCD(l(p1,p2,p3,p4,p5,p6))# 1

GCD(A,"c") # 1
list(GCD(l(p1,p2)),GCD(l(p3,p4)),LCM(l(p5,p6))) 
# 1 - x; 1 + x; 1 - 2*x^2 + x^4 

list(GCD(l(p1,p2)),GCD(l(p3,p4)),LCM(l(p5,p6))) 
list(GCD(l(p1,p2)),GCD(l(p3,p4)),LCM(l(p5,p6))) 

GCD(A,"r") # 1; 1 
list(GCD(l(p1,p3,p5)),GCD(l(p2,p4,p6))) # 1; 1

# ---

LCM(A) 
LCM(l(p1,p2,p3,p4,p5,p6))
# 0.25*x - 0.5*x^3 + 0.25*x^5

LCM(A,"c") # x - x^3
list(LCM(l(p1,p2)),LCM(l(p3,p4)),LCM(l(p5,p6)))
# x - x^2; 1 - x^2; 1 - 2*x^2 + x^4

LCM(A,"r") # x - x^3
list(LCM(l(p1,p3,p5)),LCM(l(p2,p4,p6))) 
# 0.5 + 0.5*x - 0.5*x^2 - 0.5*x^3;0.5*x - 0.5*x^2 - 0.5*x^3 + 0.5*x^4 

rm(A,p1,p2,p3,p4,p5,p6,l)
}
\keyword{manip}

