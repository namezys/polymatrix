\name{t}
\alias{t.pMatrix}
\title{Matrix transpose of a 'pMatrix' class object}
\description{Given a \code{pMatrix} class \code{x}, the call
\code{t(x)} returns the transpose of \code{x}.}

\usage{\method{t}{pMatrix}(x)}

\arguments{\item{x}{an \code{pMatrix} class object}}

\details{
The method first convert the storage method of the given \code{pMatrix} object to \code{pMdlist} class interpretation,
thereafter reflects the \code{pMatrix} on its diagonal.
}

\value{The returned value: A '\code{pMdlist}, \code{pMatrix}' class object, 
  the transposed version of the given \code{x} polynomial matrix.
  }


\seealso{The \code{t} in the \code{base} package.}

\examples{

# --- 
# case of real matrix argument

m <- matrix(1:12,3,4)
m
t(m) # the \code{base::t()} function

# --- 
# case of polynomial matrix argument

pa <- pMgen.a()
pa
dim(pa) # 2 x 3 

pm <- t(pa)
pm
dim(pm) # 3 x 2 
class(pm) #  "pMdlist" "pMatrix"

rm(m,pd) 
}

