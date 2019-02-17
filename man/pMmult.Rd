\name{\%*\%}

\alias{\%*\%}

\title{Multiplication of two \code{pMatrix} class polynomial matrices}

\description{
Matrix multipliction for \code{pMatrix} class polynomial matrices.
It multiplies the two matrices only, if they are conformable. 
}

\usage{x\%*\%y}

\arguments{
\item{x}{a \code{pMatrix} class object} 
\item{y}{a \code{pMatrix} class object} 
}

\details{The number of columns in \code{x} must be equal to number of rows in \code{y}.}

\value{A \code{pMatrix} class object which is the product of 
the \code{pMatrix} class \code{x} and \code{y} matrices.}

\seealso{\code{\link{pMatrix-package}}}

\keyword{math}

\examples{
pm<-pMgen.a()
pm\%*\%t(pm)

rm(pm)

# two constante matrices
a <- matrix(1:12,3,4)
b <- matrix(1:20,4,5)
a\%*\%b
base::"\%*\%"(a,b)

a<-M2pM(a)
b<-M2pM(b)
a\%*\%b

pM2M(a)\%*\%pM2M(b)

rm(a,b)

}

