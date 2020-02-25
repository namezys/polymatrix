\name{pVsk}
\alias{pVsk}
\title{The scalar product of two polynomial vectors}

\description{
If \code{pMy} is \code{NULL}, then the result is the sum of squares of the elements of \code{pMx}.
Otherwise equals the scalar product ot the two given polynomial vectors.}
\usage{pVsk(pMx, pMy=NULL)}
\arguments{
      \item{pMx}{a \code{polyMatrix} class row or column vector}
      \item{pMy}{a \code{polyMatrix} class row or column vector}
}

\details{The two vectors must have the same length.}

\value{A \code{polynomial} class object, which is the sum of
       the elementwise product of the two vectors.}

\seealso{\link{pMprod}}

\keyword{math}

\examples{
A <- polyMgen.d(2,2,rawData=ch2pn(c("-3","2+4*z","-z^2","1"),"z"))
A

pMcol(A,1) # ("-3" , "2 + 4*x")
pVsk(pMcol(A,1)) # "13 + 16*x + 16*x^2"

pMrow(A,2) # ("2 + 4*x" , "1")
pVsk(pMcol(A,1),pMrow(A,2)) # "-4 - 8*x"

# clean up
# rm(A)
}

