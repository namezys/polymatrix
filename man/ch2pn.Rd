\name{ch2pn}
\alias{ch2pn}
\title{Converts a \code{character} class object to a \code{polynomial} class object}
\description{
Receives a \code{character} class object -- which looks like the print image of a \code{polynomial} object --
and returns a \code{polynomial} class object.}
\usage{ch2pn(chv, symb = "x")}
\arguments{
  \item{chv}{a \code{character} class object like the print image of a \code{polynomial} object}
  \item{symb}{the required symbol of input polynom string}
  }
\details{This technical subrutin converts a \code{character} object to a \code{polynomial} object.
The "x" is the default symbol in the input string. The output is a \code{polynomial} or a list of \code{polynomial}s,
without any defined symbol. The default \code{print.polynomial} writes out the output by the symbol "x".}

\value{A \code{polynomial} class object or a \code{list} of \code{polynomial} objects.}

\seealso{\code{\link{pn2ch}}, \code{\link{pprt}}}

\keyword{utilities}

\examples{

chx <- "1 + x + x^3 + 2 * x^4 - 4 * x^5 - 15 * x^16"
p <- ch2pn(chx, "x")
# "polynomial" (as in package: 'polynom')
class(p)
# 1 + x + x^3 + 2*x^4 - 4*x^5 - 15*x^16
p
# ditto <= 'x' is the default symbol
ch2pn(chx)

# the same polynom over 'z'
chz <- "1 + z + z^3 + 2 * z^4 - 4 * z^5 - 15 * z^16"
# == 1: the only "x" power in the input string
ch2pn(chz)
# 1 + x + x^3 + 2*x^4 - 4*x^5 - 15*x^16
ch2pn(chz, "z")

# the same print by polyMatrix::pprt() print utility
pprt(ch2pn(chz, "z"), "x")

# 1 + 5*x - 5*x^2 - 2*x^3 + x^5 + x^6 - x^7
ch2pn("-2 * x^3 - 5 * x^2 + x^5 + x^6 + 5 * x - x^7 + 1")
# 1
ch2pn("-2 * x^3 - 5 * x^2 + x^5 + x^6 + 5 * x - x^7 + 1", "z")

# 2
ch2pn(" 2")
# 0
ch2pn("z^3")
# z^4
ch2pn("z^3", "z")

# 0
ch2pn(" 3 * z ^ 2 + 5 * z^3")
# 4
ch2pn(" 4  + 5 * z^3")
# 6 + 5*x^3
ch2pn(" 6  + 5 * z^3", "z")

# a list of 'polynomial' class elements: [[1]] x  [[2]] 1 + x^2
ch2pn(c(" x ", " 1 + x^2 "))

# col.wise a "polyMdlist" "polyMatrix" class object
polyMgen.d(2, 2, rawData=ch2pn(c("1", "x^2", "x", "0")))

# symbole given for output
polyMgen.d(2, 2, rawData=ch2pn(c("1", "x^2", "x", "0")), symb="x")

# symbole given for input and output
polyMgen.d(2, 2, rawData=ch2pn(c("1", "x^2", "x", "0"),symb="x"), symb="x")

# clean up
# rm(chz, p, chx)
}

