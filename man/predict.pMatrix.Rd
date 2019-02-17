\name{predict.pMatrix}
\alias{predict.pMatrix}
\title{Calculate the value of a polynomial for real, complex and matrix argument}
\description{ ... }

\usage{\method{predict}{pMatrix}(object,M,\dots)}

\arguments{\item{object}{    ~~Describe \code{polynomial} here~~}
           \item{M}{    ~~Describe \code{polynomial} here~~}
           \item{\dots}{    ~~ this parameter}
            }

\details{The 'polynom::predict.polynomial()' method works for real,
         complex and matrix argument also. But in the case of matrix
         argumet it uses the elementwise product. This modified 
         version calculate by the usual matrix product definition.}

\value{Describe the value returned!! If it is a LIST, use \item{comp1 }{Description of 'comp1'}}
\note{further notes }

\seealso{\code{\link{predict.polynomial}}, \code{\link{predict.charpn}}}

\keyword{arith}

\examples{

# predict the value of a polynomial matrix at a real or complex value
pm<-pMgen.d(2,2,
  rawData=list(polynom::polynomial(1),
               polynom::polynomial(c(0,0,1)),
               polynom::polynomial(c(0,1)),
               polynom::polynomial(0)))
pm # 1, z^2\\\\ z, 0
               
predict(pm,2) # matrix(c(1,4,2,0),2) 

# predict the result of a linear model
x<-1:5;y<-rnorm(5);predict(lm(y~x)) # the used method: predict.lm()
rm(pm,x,y)
}

