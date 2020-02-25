\name{pn2ch}
\alias{pn2ch}

\title{Converts a polynom class object to a character class object}

\description{
Takes a polynom class object and returns a character class object
looks like the print image of the polynom object.
}

\usage{pn2ch(x, symb = "x", digits = getOption("digits"), decreasing = FALSE, ...)}

\arguments{
  \item{x}{the given polynom class object}
  \item{symb}{the symbol of polynom}
  \item{digits}{the number of significant digits to be printed}
  \item{decreasing}{the desired order of the terms of the polynomial}
  \item{\dots}{additional arguments}                  }

\details{A technical subrutin to convert a polynom object to a character object.}

\value{A character class object.}

\seealso{\code{\link{pprt}}}

\keyword{utilities}

\examples{
p <- polynom::polynomial(0:12)
pn2ch(p)

p <- polynom::polynomial(12:0)
pn2ch(p)

# clean up
# rm(p)
}

