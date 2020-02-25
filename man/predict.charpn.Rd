\name{predict.charpn}
\alias{predict.charpn}
\title{Calculate the value of a characteristic polynomial for a polyMatrix argument}

\description{
The \sQuote{\code{polynom::predict.polynomial()}} method works by real coefficients
		 only but it works for real or complex and matrix argument also.
		 But in the case of matrix argument uses the elementwise product.
		 The \sQuote{\code{polyMatrix::predict}} method evaluate the polynom by
		 the usual matrix product definition.}

\usage{\method{predict}{charpn}(object,pM,...)}

\arguments{\item{object}{a \sQuote{charpn} class object}
           \item{pM}{a \sQuote{polyMatrix} class object}
           \item{\dots}{additional arguments}
		   }


\value{\item{value}{A \sQuote{polynomial} class object}}

\seealso{\code{\link{predict.polynomial}}, \code{\link{predict.polyMatrix}}}

\keyword{arith}

\examples{

pm <- polyMgen.d(2,2,rawData=ch2pn(c("1","x^2","x","0")))
pm # 1, z^2\\\\ z, 0

predict(pm,2) # matrix(c(1,4,2,0),2)

# predict the result of a linear model
x <- 1:5;y <- rnorm(5);predict(lm(y~x)) # the used method: predict.lm()

# clean up
# rm(pm, x, y)
}

