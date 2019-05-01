\name{CanForm}
\alias{CanForm}
\alias{CanForm.echelon}
\alias{CanForm.final}
\alias{CanForm.scm}
\title{Conversion between the pairs of the four polyMatrix subclasses}

\description{The \code{polyMatrix} package has 4 different but equivalent method to store
the polynomial matrices. This function provide a conversion between the 4 methods. In the
background operate one of 6 method, matching to the sub-class of the given input object 
and the required sub-class of the output object.}

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

\value{An \code{polyMatrix} class object, equivalent with the \code{pM} input object, 
       but stored in the new, given format by the \code{newclass} parameter.}

\seealso{\code{\link{polyMatrix-package}}}

\keyword{utilities}

\examples{
parma <- polyMgen.varma(3,3,rand=TRUE,degree=c(1,1))
parma

CanForm(parma,"final")
CanForm(parma,"echelon")
CanForm(parma,"scm")

rm(parma)

}

