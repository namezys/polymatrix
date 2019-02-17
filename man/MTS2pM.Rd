\name{MTS2pM}
\alias{MTS2pM}
\title{Conversion the VARMA representation of MTS package to pMatrix representation}

\description{The VARMA representation of MTS is a list by 13 elements. 
The necessary elements to represent a VARMA model are the c(2, 3, 4, 5, 8, 11, 12, 13):
ARorder, MAorder, cnst, coef, Sigma, Phi, Theta, Ph0 }

\usage{MTS2pM(M)}

\arguments{
  \item{M}{an arbitrary VARMA modell list of \code{MTS} package}   
  	}

\details{MTS (Multivariate Time Series)  
         is a general package for analyzing multivariate linear time series,
         written by Ruey S. Tsay.}

\value{An \code{pMvarma} class object, equivalent with the \code{M} input object, 
       created by the package \code{MTS}.}

\seealso{\code{\link{print.pMvarma}}, \code{\link{print.pMatrix}}}

\keyword{utilities}

\examples{
\donttest{
# runs longly
rm(list=ls())
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

rm(yt,M,pMvarma)}

}

