\name{predict.polynomial}
\alias{predict.polynomial}
\title{Calculate the value of a polynomial matrix for real or complex values}

\description{ ... }

\usage{
\method{predict}{polynomial}(object,M,meth=c("as.matrix","as.the.polynom.package"),\dots)
}

\arguments{\item{object}{a \code{numeric}, \code{matrix} or \code{polyMatrix} 
                 class object}
           \item{M}{a \code{polynomial} class object}
           \item{meth}{If the value of this parameter as the default, 
                       "as.matrix" then calculate by matrix multiplications.
                       Otherwise it calculate by elementwise multiplications.}
           \item{\dots}{    ~~ this parameter}
            }
\details{The 'polynom::predict.polynomial()' method works for real,
         complex and matrix argument also. But in the case of matrix
         argument it uses the elementwise calculations. 
         The polyMatrix::predict.polynomial() calculate by the usual 
         matrix product definition except the case, when the parametere \code{meth}
		 not equals by "as.matrix".}
\value{The class of the output is \code{numeric}, \code{matrix} or
       \code{polyMatrix}, depending on the class of the input}
\note{further notes }

\seealso{\code{\link{predict.polyMatrix}}, \code{\link{predict.charpn}}}

\keyword{arith}

\examples{
  p <- polynom::polynomial(1:3)
  p # 1 + 2*x + 3*x^2
  # predict a polynom for real values
  predict(p,1) # 6
  # predict a polynom for complex values
  predict(p,1i) # -2+2i
  # predict a polynom for matrices
  M <- matrix(c(1,-1),2,2);
  M
  predict(p,M) # matrix(c(3,-2,2,-1),2)
  
  # mimiking the elementwise calculation of the masked "polynom::predict" method
  predict(p,M,meth="as.the.polynom.package"); # matrix(c(6,2,6,2),2,2)
  
  
  # predict a polynom for polynomial matrices
  pM<-polyMgen.d(2,2, rawData=ch2pn(c("1","x^2","x","0")))
  
  pM<-polyMgen.d(2,2,rawData=ch2pn(c("1","z","z^2","0"),"z"),byrow=TRUE)
  			   
  pM # 1, z\\\\ z^2, 0
  predict(p,pM) # method: predict.polyMatrix()
  # 6 + 3*z^3  5*z       
  # 5*z^2      1 + 3*z^3
  
  # --- 
  # predict the result of a linear model
  x<-1:5;y<-rnorm(5);predict(lm(y~x)) # method: predict.lm()
  rm(p,M,pM,x,y)
  }
  
