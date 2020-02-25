\name{pMbas}
\alias{pMbas}
\title{Bastion elements of a polynomial matrix}

\description{Elements of a \code{matrix} or \code{polyMatrix} object,
         from non-beating positions for bastion in the matrix as a chess board.}

\usage{pMbas(pm, ki, byrow) }
\arguments{
  \item{pm}{a \code{polynomial} matrix object}
  \item{ki}{a permutation of the column numbers}
  \item{byrow}{logical. If \code{FALSE} (the default) the elments of \code{ki} are column indexes,
                        if \code{TRUE} these are row indexes.}   }
\details{Usual}
\value{A list of \code{polynomial} class bastion elements
       of the given \code{polyMatrix}
       determined by the given permutation \code{ki}.
       in a \code{pVector} object}
\seealso{\code{\link{pMdet}}}
\keyword{manip}
\examples{
A<-polyMgen.a()
class(A)#  "polyMarray" "polyMatrix"
dim(A)# 2 x 3
A
pMbas(A,c(2,3),byrow=TRUE) # the [1,2] and [2,3] elements
pMbas(A,c(2,2,1),byrow=FALSE) # the [2,1], [2,2]  and [1,3] elements
pMbas(A,c(2,2),byrow=FALSE) # the [2,1] and [2,2] elements
# pMbas(A,c(2,2,1),byrow=TRUE) # Error: Index vector too long!

A<-polyMgen.d(3,3,
     rawData=ch2pn(c("1","0","-1",
                    "2","1","-1",
                    "0","1","1")),byrow=TRUE)
A
pMbas(A,c(2,3,1),byrow=TRUE) # the [1,2], [2,3]  and [3,1] elements
pMbas(A,c(2,3,1),byrow=FALSE) # the [2,1], [3,2]  and [1,3] elements

# clean up
# rm(A)
}

