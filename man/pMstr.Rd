\name{pMstr}
\alias{pMstr}
\title{pMatrix object consistency check}
\description{ ... }
\usage{pMstr(pm) }
\arguments{
  \item{pm}{a polynomial matrix}   }
\details{Checks the consistency of a \code{pMatrix} object.
In the \code{pMatrix} there are four different but equivalent 
storing methods of polynomial matrices: \code{pMatrix}}
\value{
A print on the consistency of the given polynomial object.
Information about the observed errors.
}

\note{further notes }
\author{Manel Boukhris [tun]}
\seealso{See also 
\code{\link{pMgen.a}}, 
\code{\link{pMgen.b}}, 
\code{\link{pMgen.c}}, 
\code{\link{pMgen.d}}}
\examples{
A<-pMgen.a(2,3,5)
B<-pMconvert(A,"pMdlist")
\donttest{
pMstr(B)$cns

## class of elements: polynomial
B[[1]][[1]]<-1
str(B)
(pMstr(B))

## class of sublists: list
B[[1]]<-1:length(B[[1]])
str(B)
pMstr(B)

# absence of an element
B[[1]]<-as.list(B[[1]][-1])
str(B)
pMstr(B)[[3]]


} # end of donttest
}
\keyword{manip}

