\name{MTS2pM}
\alias{MTS2pM}
\title{Converts MTS representation of a VARMA process to a polyMatrix representation}

\description{The MTS representation of VARMA process is a list
of 13 elements.
The necessary elements to represent a VARMA model are the c(2, 3, 4, 5, 8, 11, 12, 13) elements:
ARorder, MAorder, cnst, coef, Sigma, Phi, Theta, Ph0.
The other five are redundant parameters.}

\usage{MTS2pM(M)}

\arguments{
  \item{M}{an arbitrary VARMA model list-object of the \code{MTS} package}
  	}

\details{MTS (Multivariate Time Series)
         is a general package for analyzing multivariate linear time series,
         written by Ruey S. Tsay.}

\value{An \code{pMvarma} class object, equivalent with the \code{M} input object,
       created by the package \code{MTS}.}

\seealso{\code{\link{print.pMvarma}}, \code{\link{print.polyMatrix}}}

\keyword{utilities}

\examples{
\donttest{
# runs longly
library(MTS)
set.seed(1)
yt <- VARMAsim(300,arlags=1,malags=1,
        phi=matrix(c(0.2,-0.6,0.3,1.1),2,2),
        theta=matrix(c(-0.5,0,0,-0.5),2,2),
        sigma=diag(2))$series
M <- VARMA(yt,p=2,q=2,include.mean=TRUE)
M[-c(1,6,7,9,10)]
pMvarma<-MTS2pM(M)
print(pMvarma,style="broad",digits=3)
pMvarma
print(pMvarma,"matrix",digits=3)

# clean up
# rm(yt, M, pMvarma)}

}

