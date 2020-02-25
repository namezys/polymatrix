\name{lead}
\alias{lead}
\title{The leading coefficient of a polynom or polynomial matrix}
\description{
Returns the leading coefficients of the
input polynom or polynomial matrix.
}

\usage{lead(p,method=c("matrix","column","row","element"))}
\arguments{
\item{p}{a \code{\link{polynomial}} or \code{\link{polyMatrix-package}} class object}
\item{method}{controls the interpretation of the word: "leading", see the details}
           }
\details{
The four possible interpretation of the \code{lead}
for a polynomial matrix are:
\itemize{

\item{\code{matrix}: the highest matrix coefficient of the polynomial matrix.}
\item{\code{column}: taken the polynomial matrix column-wise,
the row vector of the highest coefficient of each column.}
\item{\code{row}: taken the polynomial matrix row-wise, the
column vector of the highest coefficient of each row.}
\item{\code{element}: a matrix formed by the highest coefficients of the polynomial elements of the polynomial matrix.}
}
}

\value{
Returns a matrix by the same size as the polynomial matrix.
}

\seealso{
\code{\link{coefs}}, \code{\link{degree}}, \code{\link{const}}
}

\keyword{utilities}

\examples{
lead(polynom::polynomial(0:12))
pm <- polyMgen.a()
pa <- polyMconvert(pm,"polyMarray")
pb <- polyMconvert(pm,"polyMbroad")
pc <- polyMconvert(pm,"polyMcells")
pd <- polyMconvert(pm,"polyMdlist")
lead(pa)
lead(pb)
lead(pc)
lead(pd)

rawAdat <- ch2pn(c("-3 + x^2","2 + 4*x","-x^2","1","2","3 + x","2*x","0","2 - 3*x" ))
px <- polyMgen.d(3,3,rawData=rawAdat)

# clean up
# rm(rawAdat)

px

lead(px)
lead(px,method="matrix")
lead(px,method="row")
lead(px,method="column")
lead(px,method="element")

# clean up
# rm(pm, pa, pb, pc, pd, px)
}




