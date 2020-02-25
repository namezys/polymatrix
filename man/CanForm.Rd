\name{CanForm}
\alias{CanForm}
\alias{CanForm.echelon}
\alias{CanForm.final}
\alias{CanForm.scm}

\title{Conversion of an ARMA representation to a canonical form}

\description{
The representation of an ARMA processes is unambiguous only in canonical form.
The \code{CanForm} method, if the necessary parameters are given
converts the ARMA representation to
\code{echelon}, \code{final} or \code{scm} form.
}

\usage{
CanForm(pM,form)
CanForm.echelon(pM)
CanForm.final(pM)
CanForm.scm(pM)
}

\arguments{
\item{pM}{an arbitrary \code{pMvarma} class object}
\item{form}{an arbitrary or "\code{echelon}", "\code{final}", "\code{scm}" form code}
}

\value{
An \code{pMvarma} class object in echelon, final or scm form.
}

\note{
This method doesn't work yet - is not implemented yet.
}

\seealso{
\code{\link{polyMatrix-package}}
}

\keyword{utilities}

\examples{
parma <- polyMgen.varma(3,3,rand=TRUE,degree=c(1,1))
parma

CanForm(parma,"final")
CanForm(parma,"echelon")
CanForm(parma,"scm")

# clean up
# rm(parma)
}

% ----
% fine
