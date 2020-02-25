\name{pprt}
\alias{pprt}
\title{Intelligent print of a \code{polynom} object}

\description{
Calls the \code{pn2ch} converter, and prints the polynomial
considering \code{shift} parameter and the width of the actual consol window.}

\usage{pprt(x, symb = "x", shift = 0, digits = getOption("digits"), decreasing = FALSE, ...)}

\arguments{
  \item{x}{the polynom object to be printed}
  \item{symb}{the symbol used in the print}
  \item{shift}{the beginnig shift}
  \item{digits}{the width of the coefficients to be printed}
  \item{decreasing}{indicator of the order of powers terms: decreasing or not}
  \item{\dots}{additional arguments}                     }

\seealso{\code{\link{pn2ch}}}

\keyword{print}

\examples{
p <- polynom::polynomial(0:12)
pprt(p)
pprt(p,sh=3)
pprt(p,sh=3,symb="L")
# clean up
# rm(p)
}

