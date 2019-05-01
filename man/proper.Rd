\name{proper}
\alias{proper}

\title{The polynomial matrix is column//row proper or not}

\description{ ... }

\usage{proper(pm,type=c("col","row","both"),print=c(TRUE,FALSE))
}

\arguments{\item{pm}{a \code{polyMatrix} class polynomial matrix}
           \item{type}{the type of rating \code{row}//\code{col}//\code{both}}                 
           \item{print}{the kind of printing}}                 


\details{The program return the answer, wether the given polynomial matrix column or row proper. 
By other notion column or row reduced. First calculate the column or row associated matrix,
and the answer depends on its rank. The program has an \code{invisible} silent output.}

\value{Logical: \code{TRUE} or \code{FALSE}. 
The associated matrix is only a optional part of the output, and the print.}

\seealso{\code{\link{coefs}}, \code{\link{lead}}}

\keyword{utilities}

\examples{
pm<-polyMgen.d(2,2,rawData=ch2pn(c("-1+7*x","x","3-x+x^2","-1+x^2-3*x^3")))
pm
proper(pm) # the default is the col-property labeling
(proper(pm,print=FALSE)) # the invisible output, without printing
proper(pm,"col")
proper(pm,"row")
proper(pm,"both")

rm(pm)
}

