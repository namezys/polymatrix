\name{pMstr}
\alias{pMstr}

\title{polyMatrix object consistency check}

\description{
Checks the consistency of a \code{polyMatrix} object.
In the \code{polyMatrix} there are four different but equivalent
storing methods of polynomial matrices.
}

\usage{pMstr(pm) }

\arguments{
\item{pm}{a polynomial matrix}
}

\value{
A logical value of the consistency of the given polynomial object.
In case of inconsistency an information about the errors.
}

\seealso{
\code{\link{polyMgen.a}},
\code{\link{polyMgen.b}},
\code{\link{polyMgen.c}},
\code{\link{polyMgen.d}}
}

\examples{
A <- polyMgen.a(2,3,5)
B <- polyMconvert(A,"polyMdlist")
\donttest{
# pMstr(B)$cns

## class of elements: polynomial
# B[[1]][[1]]<-1
# str(B)
# (pMstr(B))

## class of sublists: list
# B[[1]]<-1:length(B[[1]])
# str(B)
# pMstr(B)

# absence of an element
# B[[1]]<-as.list(B[[1]][-1])
# str(B)
# pMstr(B)[[3]]

# clean up
# rm(A, B)

} # % end of donttest
}
\keyword{manip}

