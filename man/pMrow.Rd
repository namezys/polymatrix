\name{pMrow}
\alias{pMrow}
\title{A row of the given \code{polyMatrix}}

\description{
Picks up a given row of the given \code{polyMatrix}.
}

\usage{pMrow(pm, which = 1)}
\arguments{
  \item{pm}{a \code{polyMatrix} class object}
  \item{which}{the serial number of the required column} }

\value{
A \code{pVector} class list of the elements of
the \code{which} row of the \code{pm} polynomial matrix.
}

\seealso{\code{\link{pMrow}}}

\keyword{manip}

\examples{
A <- polyMgen.a()
A
pMrow(A,2)

# clean up
# rm(A)
}

