\name{pMcol}
\alias{pMcol}

\title{
A column of the given \code{polyMatrix}
}

\description{
Picks up a given column of the given matrix}

\usage{pMcol(pm, which = 1)}

\arguments{
  \item{pm}{a \code{polyMatrix} class object}
  \item{which}{the serial number of the required column}     }

\value{A \code{pVector} class list of the elements of
       the column \sQuote{\code{which}} of the polynomial matrix \sQuote{\code{pm}}.}

\seealso{\code{\link{pMrow}}}

\keyword{manip}

\examples{
A <- polyMgen.a()
A

pMcol(A,2)

# clean up
# rm(A)
}

