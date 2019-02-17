\name{pMcol}
\alias{pMcol}
\title{A column of the given \code{pMatrix}}
\description{Pick up a given column of the given matrix}
\usage{pMcol(pm, which = 1)}
\arguments{
  \item{pm}{a \code{pMatrix} class object}
  \item{which}{the number of the required column}     }
\details{more details than the description above }
\value{A \code{pVector} class list of the elements of 
       the \code{which} column of the \code{pm} polynomial matrix}
\seealso{\code{\link{pMrow}}}
\keyword{manip}
\examples{
A<-pMgen.a()
A
pMcol(A,1)
}

