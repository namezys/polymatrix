\name{predict.charpn}
\alias{predict.charpn}
\title{Calculate the value of a characteristic polynomial for a polyMatrix argument}

\description{ ... }

\usage{\method{predict}{charpn}(object,pM,...)}

\arguments{\item{object}{a 'charpn' class object}
           \item{pM}{a 'polyMatrix' class object}            
           \item{\dots}{...}            
		   }

\details{The 'polynom::predict.polynomial()' method works by real coefficients
		 only but works for real or complex and matrix argument also. 
		 But in the case of matrix argument it uses the elementwise product. 
		 This 'predict' method calculate by the usual matrix product definition.}

\value{\item{value}{A 'polinomial' class object}}


\seealso{\code{\link{predict.polynomial}}, \code{\link{predict.polyMatrix}}}

\keyword{arith}

\examples{

pm<-polyMgen.d(2,2,rawData=ch2pn(c("1","x^2","x","0")))
pm # 1, z^2\\\\ z, 0
               
predict(pm,2) # matrix(c(1,4,2,0),2) 

# predict the result of a linear model
x<-1:5;y<-rnorm(5);predict(lm(y~x)) # the used method: predict.lm()
rm(pm,x,y)
}

