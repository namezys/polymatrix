\name{tr}
\alias{tr}

\title{Trace of a 'matrix' or 'pMatrix' class matrix}

\description{Returns the trace of the given matrix.}

\usage{tr(M)}

\arguments{\item{M}{a \code{matrix} or \code{pMatrix} class object}}

\details{Calculate the sum of the diagonal elements of the given matrix.}

\value{\itemize{
  \item a \code{numeric} class object, 
        if the given matrix is a \code{matrix} class object.
  \item a \code{polynomial} class object, 
        if the given matrix is a \code{pMatrix} class object.    }}

\seealso{\code{\link{tr.pMatrix}}, \code{\link{pMatrix-package}}, \code{\link{polynomial}}, 
         but the \code{\link{trace}} is a debugging utility}

\keyword{utilities}

\examples{

# the case of a matrix class input object
M <- matrix(1:9,3,3)
class(M)
M
# 1  4  7
# 2  5  8
# 3  6  9
tr(M) # 15

(M <- matrix(1:12,3,4))
# 1  4  7  10
# 2  5  8  11
# 3  6  9  12
tr(M) # 15

(M <- matrix(1:12,4,3))
# 1  5   9
# 2  6  10
# 3  7  11
# 4  8  12
tr(M) # 18

# case of pMatrix class input objects
pM <- pMgen.d(3,3,rawData=
       ch2pn(c("-3 + z^2","2 + 4*z","-z^2",
               "1",       "2","3 + z",
               "2*z","0","2 - 3*z"),"z"),byrow=TRUE,symb="z")
class(pM) # "pMdlist" "pMatrix"
pM

( p <- tr(pM) ) # 1 - 3*x + x^2 
class(p) # "polynomial"

rm(M,pM,p)

}


