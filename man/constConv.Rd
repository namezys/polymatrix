\name{constConv}
\alias{M2pM}
\alias{pM2M}

\title{Conversion of constant matrices}

\description{
Conversion between the 'matrix' and 'polyMatrix' class representation of constant matrices.
}

\usage{
M2pM(m,class=c("polyMarray","polyMbroad","polyMcells","polyMdlist"))
pM2M(pM)
}

\arguments{
\item{m}{A matrix class object}
\item{class}{The required class of the value}
\item{pM}{A polyMatrix class object}                 
}

\details{\code{M2pM} convert the \code{matrix} class objets to an \code{polyMatrix} class object.
\code{pM2M} convert the zero degree \code{polyMatrix} class objects  to an \code{matrix} object.}

\value{
The \code{M2pM(m)} returns the given 'm' matrix in 'polyMatrix' class form.
The \code{pM2M(pM)} returns the given 'pM' polynomial matrix in 'matrix' class form, if it is a constant matrix,
else give an error sign.
}

\seealso{\code{\link{coefs}}, \code{\link{lead}},  \code{\link{degree}}}

\keyword{utilities}

\examples{

m<-matrix(1:12,3,4)
m
pM<-M2pM(m)
class(pM)
pM

M2pM(m,"polyMarray")
M2pM(m,"polyMbroad")
M2pM(m,"polyMcells")
M2pM(m,"polyMdlist")

rm(m,pM)

}

