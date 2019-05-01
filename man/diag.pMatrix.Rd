\name{diag.polyMatrix}
\alias{diag.polyMatrix}
\alias{diag.list}

\title{Extract the diagonal of a polyMatrix, or construct a diagonal polyMatrix}

\description{ ... }

\usage{
\method{diag}{list}(v,k,j=k)
\method{diag}{polyMatrix}(pm,k=dim(pm)[1],j=dim(pm)[2],type=c("list","polyMatrix"))
}

\arguments{
\item{v}{the \code{list} class input material for the composed diagonal matrix}
\item{k}{number of rows in the output object}
\item{j}{number of columns in the output object}
\item{pm}{the \code{polyMatrix} class input matrix}
\item{type}{the required form and class of the result}
}

\details{
The \code{diag} has two distinct usages.
When the class of the first input parameter is
\itemize{
    \item {'\code{list}', then returns a polyMatrix with the given diagonal and 
    zero off-diagonal entries.}
    \item {'\code{polyMatrix}', then it extracts the diagonal.}
}
Note. 
In the first case the elements of the list must be \code{polynomial} class objects.   
In the second case the result is a \code{list} or \code{polyMatrix} class object
determined by the value of the \code{type} parameter.
}

\value{The returned value is a 
  \item{list}{class object, when it extracts the diagonal from a \code{polyMatrix}, and the 
  value of \code{type} isn't \code{"polyMatrix"}.}
  \item{polyMatrix}{class object, when it compose a matrix from the given diagonal or when it
  catch the diagonal of a \code{polyMatrix}, and \code{type="polyMatrix"}.}
  }

\seealso{\code{\link{polyMatrix-package}}.}

\keyword{internal}

\examples{

# --- 
# case of real matrices

m<-matrix(1:12,3,4)
m
diag(m) # first type usage

v<-c(1,5,9) # second type usage
diag(v)
diag(v,4)
diag(v,4,2)
diag(v,ncol=4)

# --- 
# case of polynomial matrices

# first kind usage: catch the diagonal elements

px<-polyMgen.d(3,3,rawData=ch2pn(
     c("-3 + z^2","2 + 4*z","-z^2",
       "1","2","3 + z",
       "2*z","0","2 - 3*z"),"z"),byrow=TRUE)
class(px) # "polyMdlist" "polyMatrix"
px

w<-diag(px) # the default: type="list"
class(w) # "list"
# List of 3
# $ :Class 'polynomial'  num [1:3] -3 0 1
# $ :Class 'polynomial'  num 2
# $ :Class 'polynomial'  num [1:2] 2 -3
w

w<-diag(px,type="polyMatrix")
class(w) # "polyMdlist" "polyMatrix"
w


# second kind usage: compose a diagonal polyMatrix

ci<-c(4,1,3,2)
v<-vector("list",length(ci))
for(i in 1:length(ci))
  v[[i]]<-polynom::polynomial(c(rep(0,ci[i]),1))
class(v) # "list"
v

diag(v)
diag(v,4)
diag(v,3)
diag(v,5)
diag(v,3,5)

pd<-diag(v,4,5) 
class(pd) # "polyMdlist" "polyMatrix"
pd

rm(ci,i,m,pd,px,v,w)

}

