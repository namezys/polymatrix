\name{predict.polynomial}
\alias{predict.polynomial}
\title{Calculate the value of a \sQuote{\code{polynomial}} for real, complex and matrix argument}

\description{
The \sQuote{\code{polynom::predict.polynomial()}} method
works for real, complex and matrix argument also.
But in the case of matrix argument it uses the elementwise
calculations instead the matrix multiplication.
The \sQuote{\code{polyMatrix::predict.polynomial()}} calculate the result
by the usual matrix product definition except the case, when
the parameter \code{meth} equals by "as.in.the.polynom.package".}

\usage{
\method{predict}{polynomial}(object,M,meth=c("as.matrix","as.in.the.polynom.package"),\dots)
}

\arguments{\item{object}{a \code{numeric}, \code{matrix} or \code{polyMatrix}
                 class object}
           \item{M}{a \code{polynomial} class object}
           \item{meth}{If the value of this parameter as the default,
                       "as.matrix" then calculate by matrix multiplications.
                       Otherwise it calculate by elementwise multiplications.}
           \item{\dots}{additional arguments}
            }
\value{The is a \code{numeric}, \code{numeric}, \code{matrix} or \code{polyMatrix}
       class object depending on the class of the input}

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
predict(p,M) # 3,2 \\ -2, -1

# mimiking the elementwise calculation of the masked "polynom::predict" method
predict(p,M,meth="as.in.") # 6, 6 \\ 2, 2

# predict a polynom for a polynomial matrix
pM <- polyMgen.d(2,2, rawData=ch2pn(c("1","x^2","x","0")))
pM # 1, x \\ x^2, 0
predict(p,pM) # method: predict.polyMatrix()
# 6 + 3*z^3  5*z
# 5*z^2      1 + 3*z^3

# ---
# predict the result of a linear model
x <- c(1,3,4,1,0);
y <- c(27,81,54,27,18)
predict(lm(y~x)) # method: predict.lm()
# 31 57 70 31 18

# clean up
# rm(p, M, pM, x, y)
}

