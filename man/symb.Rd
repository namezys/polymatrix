\name{symb}
\alias{symb}
\title{The printing symbol of the given polynomial matrix}
\description{Get the printing symbol of the given polynomial matrix}

\usage{symb(pm)}
\arguments{
  \item{pm}{a \code{polyMatrix} class object}
}

\value{One character, which used to print the polynomials in default cases}

\details{
  The polynomial matrix objects contains a character to sign the variable
  of the polynomials. The system default is '\code{x}'.
}

\seealso{
  \code{\link{dim.polyMatrix}},
  \code{\link{degree.polyMatrix}},
  \code{\link{degree.polynomial}}
}

\keyword{utilities}

\examples{

pM <- polyMgen.d(2,2,rawData=
   ch2pn(c("-3+s^3","2+4*s","s^2","1"),symb="s"))
pM

# -3 + x^3   x^2
#  2 + 4*x    1

symb(pM) # "x"

pM <- polyMgen.d(2,2,rawData=
   ch2pn(c("-3+x^3","2+4*x","x^2","1")),symb="L")
pM
# -3 + L^3   L^2
#  2 + 4*L    1

symb(pM) # "L"

# clean up
# rm(pM)
}

