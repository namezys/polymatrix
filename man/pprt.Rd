\name{pprt}
\alias{pprt}
\title{Intelligent print of a \code{polynom} object}
\description{ ... }
\usage{pprt(x, symb = "x", shift = 0, digits = getOption("digits"), decreasing = FALSE, ...)}
\arguments{
  \item{x}{The polynom object to print}
  \item{symb}{The symbol used in the print}
  \item{shift}{The beginnig shift}
  \item{digits}{The width of the coefficients print}
  \item{decreasing}{Indicator of the rank of powers: decreasing or not}
  \item{\dots}{Additional arguments}                     }
\details{Calls the \code{pn2ch} converter, and depending on the beginning shift and the width of the actual consol window print out a polynom object}
\seealso{\code{\link{pn2ch}}}
\keyword{print}
\examples{
p<-polynom::polynomial(0:12)
pprt(p)
pprt(p,sh=3)
pprt(p,sh=3,symb="L")
rm(p)
}

