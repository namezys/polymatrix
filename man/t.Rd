\name{t}
\alias{t.polyMatrix}
\title{Matrix transpose of a 'polyMatrix' class object}
\description{
  Given a \code{polyMatrix} class \code{x},
  the call \code{t(x)} returns the transpose of \code{x}.
}

\usage{\method{t}{polyMatrix}(x, ...)}

\arguments{
  \item{x}{an \code{polyMatrix} class object}
  \item{...}{not used}
}

\details{
  The method first convert the storage method of
  the given \code{polyMatrix} object to \code{polyMdlist} class interpretation,
  then flips the \code{polyMatrix} over its diagonal.
}

\value{
  A '\code{polyMdlist}, \code{polyMatrix}' class object,
  the transposed version of the given \code{x} polynomial matrix.
}

\seealso{
  The \sQuote{\code{t}} in the \code{base} package.
}

\examples{
# ---
# case of real matrix argument

m <- matrix(1:12,3,4)
m
t(m) # the \code{base::t()} function

# ---
# case of polynomial matrix argument

pa <- polyMgen.a()
pa
dim(pa) # 2 x 3

pm <- t(pa)
pm
dim(pm) # 3 x 2
class(pm) #  "polyMdlist" "polyMatrix"

# clean up
# rm(m,pd)
}
