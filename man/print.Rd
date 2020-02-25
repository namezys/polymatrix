\name{print}
\alias{print.polyMatrix}
\alias{print.pMvarma}
\title{Prints a polynomial matrix or a varma model in the given form}

\description{Printing method for class \code{polyMatrix} and \code{pMvarma}.}

\usage{
\method{print}{polyMatrix}(
    x, style=c("matrix", "polynom", "broad", "raw"),
    round=NULL,digits=getOption("digits"), shift = 3, decreasing = FALSE, ...
)
\method{print}{pMvarma}(
    x, style=c("matrix", "polynom", "broad", "raw"),
    round=NULL,digits=getOption("digits"), shift = 3, ...
)
}

\arguments{\item{x}{a \code{polyMatrix} or \code{pMvarma} class object to be printed}
           \item{style}{one of the four printing types}
           \item{round}{rounds as the \code{base::round()} function}
           \item{digits}{the number of significant digits to be printed}
           \item{shift}{the beginnig shift}
           \item{decreasing}{the terms of the polynomial in decreasing order or not}
		   \item{\dots}{additional arguments} }


\seealso{\code{\link{print}}}

\keyword{print}

\examples{
set.seed(1)
pm <- polyMgen.a(rand=function(x) rpois(x,1))


pa <- polyMconvert(pm,"polyMarray") # class: "polyMarray" "polyMatrix"
pb <- polyMconvert(pm,"polyMbroad") # class: "polyMbroad" "polyMatrix"
pc <- polyMconvert(pm,"polyMcells") # class: "polyMcells" "polyMatrix"
pd <- polyMconvert(pm,"polyMdlist") # class: "polyMdlist" "polyMatrix"
# clean up
# rm(pm)

# the default, the "matrix" print image style: matrix of polynomials
pa
pb
pc
pd

# the three other print image style:
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

# clean up
# rm(pa,pb,pc,pd)

varma <- polyMgen.varma()

varma
print(varma,"matrix") # the same print image
print(varma,"poly")
print(varma,"broad")
print(varma,"raw")

# clean up
# rm(varma)

}



