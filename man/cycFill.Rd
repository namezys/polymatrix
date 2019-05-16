\name{cycFill}
\alias{cycFill}
\title{A given length vector or list, filled cyclical by given material}
\description{Fills an object by the given material}

\usage{cycFill(data, size)}
\arguments{
  \item{data}{the material}
  \item{size}{the desired length}
}

\details{The class of the output is just the same as the class of input.
The output object filled cylical filled by the given material \code{u}.}
\value{Depending on the class of the given material,
the class of the result is \code{vector} or \code{list}.
The length of the result equals by the given \code{m}}
\seealso{Used in the code of \code{\link{polyMgen.a}}, \code{\link{polyMgen.d}}}
\keyword{utilities}
\examples{

u<-c(4,1,3,2)
cycFill(u,2)
cycFill(u,4)
cycFill(u,6)
cycFill(u,11)
cycFill(u,12)

u<-as.list(c(4,1,3,2))
cycFill(u,2)
cycFill(u,4)
cycFill(u,6)
cycFill(u,11)
cycFill(u,12)

}

