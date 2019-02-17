\name{print}
\alias{print.pMatrix}
\alias{print.pMvarma}
\title{Print a polynomial matrix or a varma model in the given form}

\description{Printing method for class \code{pMatrix} and \code{pMvarma}.}

\usage{
\method{print}{pMatrix}(x, style = c("matrix","polynom","broad","raw"),digits = getOption("digits"), shift = 2, ...)
\method{print}{pMvarma}(x, style = c("matrix","polynom","broad","raw"),digits = getOption("digits"), shift = 2, ...)
}

\arguments{\item{x}{a \code{pMatrix} or \code{pMvarma} class object to print}
           \item{style}{one of the four printing type}
           \item{digits}{the number of significant digits to print}
           \item{shift}{shift for print of elements if the style is 'polynom'} 
		   \item{\dots}{...} }

\details{more details than the description above }

% \value{Describe the value returned!!}

\seealso{\code{\link{print}}}

\keyword{print}

\examples{
set.seed(1)
pm<-pMgen.a(rand=function(x) rpois(x,1)) 


pa <- pMconvert(pm,"pMarray") # class: "pMarray" "pMatrix"
pb <- pMconvert(pm,"pMbroad") # class: "pMbroad" "pMatrix"
pc <- pMconvert(pm,"pMcells") # class: "pMcells" "pMatrix"
pd <- pMconvert(pm,"pMdlist") # class: "pMdlist" "pMatrix"
rm(pm)

# the default print image: matrix of polynomials
pa
pb
pc
pd

print(pa,"poly")
print(pa,"broad")
print(pa,"raw")

print(pb,"poly")
print(pb,"broad")
print(pb,"raw")

print(pc,"poly")
print(pc,"broad")
print(pc,"raw")

print(pd,"poly")
print(pd,"broad")
print(pd,"raw")

rm(pa,pb,pc,pd)

varma<-pMgen.varma() 

varma
print(varma,"matrix") # the same print image
print(varma,"poly")
print(varma,"broad")
print(varma,"raw")

rm(varma)

}



