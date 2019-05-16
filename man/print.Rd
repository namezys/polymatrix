\name{print}
\alias{print.polyMatrix}
\alias{print.pMvarma}
\title{Print a polynomial matrix or a varma model in the given form}

\description{Printing method for class \code{polyMatrix} and \code{pMvarma}.}

\usage{
\method{print}{polyMatrix}(
    x, style=c("matrix", "polynom", "broad", "raw"),
    digits=getOption("digits"), shift = 2, ...
)
\method{print}{pMvarma}(
    x, style=c("matrix", "polynom", "broad", "raw"),
    digits=getOption("digits"), shift = 2, ...
)
}

\arguments{\item{x}{a \code{polyMatrix} or \code{pMvarma} class object to print}
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
pm<-polyMgen.a(rand=function(x) rpois(x,1))


pa <- polyMconvert(pm,"polyMarray") # class: "polyMarray" "polyMatrix"
pb <- polyMconvert(pm,"polyMbroad") # class: "polyMbroad" "polyMatrix"
pc <- polyMconvert(pm,"polyMcells") # class: "polyMcells" "polyMatrix"
pd <- polyMconvert(pm,"polyMdlist") # class: "polyMdlist" "polyMatrix"
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

varma<-polyMgen.varma()

varma
print(varma,"matrix") # the same print image
print(varma,"poly")
print(varma,"broad")
print(varma,"raw")

rm(varma)

}



