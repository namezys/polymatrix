\name{diag}
\alias{diag}

\title{
Extracts the diagonal of a \code{polyMatrix},
or constructs a diagonal \code{polyMatrix}
}

\description{
If the input is a \code{polyMatrix} then the output is its diagonal.
If the input is a vector then the output is a diagonal matrix.
}

\usage{
  diag(x, nrow=NULL, ncol=NULL, names=NULL, type=CLASS_LIST, ...)
}

\arguments{
  \item{x}{input material for the diagonal matrix}
  \item{nrow}{number of rows in the output object}
  \item{ncol}{number of columns in the output object}
  \item{names}{
    (when 'x' is a matrix) logical indicating if the resulting
    vector, the diagonal of 'x', should inherit 'names' from
    'dimnames(x)' if available.
  }
  \item{type}{the required form and class of the result: \code{CLASS_LIST} or \code{CLASS_MATRIX}}
  \item{\dots}{...}
}

\details{
  The \code{diag} has two different applications.
  When the class of the first input parameter is
  \itemize{
    \item {
      '\code{list}', then returns a \code{polyMatrix} with the given diagonal and
      zero off-diagonal entries.
    }
    \item {'\code{polyMatrix}', then extracts the diagonal.}
  }
  Note.
  In the first case the elements of the list must be \code{polynomial} class objects.
  In the second case the result is a \code{list} or \code{polyMatrix} class object
  determined by the value of the \code{type} parameter.
}

\value{
The returned value is a
\item{list}{class object, when it extracts the diagonal from
a \code{polyMatrix}, and the value of \code{type} isn't '\code{polyMatrix}'.}
\item{polyMatrix}{class object, when builds a matrix from
the given diagonal or
when returns the diagonal of a \code{polyMatrix},
and the  \code{type="polyMatrix"}.}
}


\seealso{\code{\link{polyMatrix-package}}.}

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

# clean up
# rm(ci, i, m, pd, px, v, w)

}

