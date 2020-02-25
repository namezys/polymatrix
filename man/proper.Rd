\name{proper}
\alias{proper}

\title{The polynomial matrix is column//row proper or not}

\description{
The program returns a logical value,
whether the given polynomial matrix is column or row proper or both.
A \sQuote{proper} matrix called in other way column or row \sQuote{reduced}.
}

\usage{proper(pm,type=c("col","row","both"),print=c(TRUE,FALSE))
}

\details{
The program calculates first the column or row associated matrices,
and the answer depends on there ranks.
A polynomial matrix is \sQuote{proper} if the associeted matrix has a full rank.
The program has an \code{invisible} silent output.
}

\arguments{
\item{pm}{a \code{polyMatrix} class polynomial matrix}
\item{type}{the type of rating \code{row}//\code{col}//\code{both}}
\item{print}{the kind of printing}
}

\value{
Logical: \code{TRUE} or \code{FALSE}.
The associated matrix is only an optional part of the output, and the print.
}

\seealso{\code{\link{coefs}}, \code{\link{lead}}}

\keyword{utilities}

\examples{
pm <- polyMgen.d(2,2,rawData=ch2pn(c("-1+7*x","x","3-x+x^2","-1+x^2-3*x^3")))
pm
proper(pm) # the default is the col-property labeling
(proper(pm,print=FALSE)) # the invisible output, without printing
proper(pm,"col")
proper(pm,"row")
proper(pm,"both")

# clean up
# rm(pm)
}

