\name{polyMgen.a}
\alias{polyMgen.a}
\alias{polyMgen.b}
\alias{polyMgen.c}
\alias{polyMgen.d}
\alias{polyMgen.varma}
\title{Generate a polyMatrix class polynomial matrix with storage method
       'array', 'broad-matrix', 'list of cells' or 'double list'}
\description{A primitive tool to generate a polynomial matrix in 'polyMatrix' form.
             The 'polyMatrix' objects independently on the storage method has three
             invariant element: the \code{$dim}, \code{$degree} and \code{$symb}.
             The \code{$dim} is the size of the matrix.
             The \code{$degree} is a matrix of non-negetive numbers, the degree of the
             polynomial elements of the polynomial matrix.
             The \code{$symb} the symbol to print and identify the reference to
             the algebraic set over which the polynom defined -- sorry, this option
             not fully works.}

\usage{
polyMgen.a(nrow=2, ncol=3, rawData=1:(nrow * ncol * (max(degree) + 1)),
           symb="x", rand=FALSE, degree=2, byrow=FALSE)
polyMgen.b(nrow=2, ncol=3, rawData=1:(nrow * ncol * (degree + 1)),
           symb="x", rand=FALSE, degree=2, byrow=FALSE)
polyMgen.c(nrow=2, ncol=3, rawData=1:(nrow * ncol * (max(degree) + 1)),
           symb="x", rand=FALSE, degree=2, byrow=FALSE)
polyMgen.d(nrow=2, ncol=3, rawData=NULL,
           symb="x", rand=NULL, degree=NULL, byrow=FALSE)
polyMgen.varma(nrow=3, ncol=3, rawData=list(polynomial(1:3)),
               symb="x", rand=FALSE, degree=c(1,1), byrow=FALSE)
}

\arguments{
  \item{nrow}{the desired number of rows.}
  \item{ncol}{the desired number of columns.}
  \item{degree}{the desired degrees of polynomials when it is random filled.}
  \item{rawData}{material to fill the polynomial matrix.
                In case of \code{polyMgen.a}, \code{polyMgen.b} and \code{polyMgen.c}
                a vector of the required coefficients of the polynomials.
                The case of \code{polyMgen.d} is different: here the \code{rawData} is
                a list of \code{\link{polynomial}} class objects.}
  \item{symb}{the desired polynom symbol.}
  \item{rand}{the name of wanted random generator to generate the
              coefficients of a random polynomial matrix.}
  \item{byrow}{logical. If \code{FALSE} (the default) the matrix is filled by columns, if \code{TRUE} the matrix is filled by rows.}
}

\details{A polynomial matrix from the given material \code{rawData} or
         (the default) or random values
the four possible storage methods are:
\itemize{
\item 'array', when the coefficient matrices of the polinomial matrix stored in two
      independent element of the list of the \code{polyMatrix}: the constant matrix in
      \code{$const}, and the coefficients of the first, second... degree of the polynomial
      in the first, second,... element of the \code{$array}.
      In this case the extra class of the generated \code{polyMatrix} object is \code{polyMarray}.
\item 'broad-matrix', when the coefficients are stored in one broad matrix in the \code{$broad}
      element of the \code{polyMatrix} object. The size of this broad matrix is: \code{nx(m*(k+1))}, when the degree of the \code{nxm} size polynomial matrix is \code{d}.
      In this case the extra class of the generated \code{polyMatrix} object is \code{polyMbroad}.
\item 'list of cells', when the \code{nxm} size coefficient matrices are stored
      in \code{d+1} element of the \code{$cells} list element of the \code{polyMatrix} object.
      In this case the extra class of the generated \code{polyMatrix} object is \code{polyMcells}.
\item 'double list', when the elements of the polynomial matrix are stored in the form
      of \code{polynomial} class objects in a \code{nxm} element double list.
      In this case the extra class of the generated \code{polyMatrix} object is \code{polyMdlist}.
}

         }
\value{A \code{polyMatrix} class object with a extra class
       \code{polyMarray}, \code{polyMbroad}, \code{polyMcells} or \code{polyMdlist},
	   which signs the storage method.}

\seealso{\code{\link{polyMgen}}}

\keyword{internal}

\examples{

polyMgen.a() # default size & coefficients & rank

# given coefficients & default rank
polyMgen.a(2, 3, 1:6)
# given coefficients & rank
polyMgen.a(2, 3, 1:6, degree=1)
# given coefficients & ranks
polyMgen.a(2, 3, 1:36, degree=matrix(0:5, 2, 3))
# given coefficients & ranks
polyMgen.a(2, 3, 1:12, degree=matrix(2:0, 2, 3))

polyMgen.a(rand=TRUE) # normally distributed coefficients

polyMgen.a(rand=rexp) # exponentially distributed coefficients

pois.vg.fv <- function(x) rpois(x,1)
polyMgen.a(rand=pois.vg.fv) # Poisson(1) distributed coefficients

# clean up
# rm(pois.vg.fv)

polyMgen.b() # class="polyMbroad", broad matrix form
polyMgen.c() # class="polyMcells", list of coefficient matrices
polyMgen.d() # class="polyMdlist", dubble list

polyMgen.d(2,2, rawData=list(
  polynom::polynomial(1), polynom::polynomial(c(0,0,1)),
  polynom::polynomial(c(0,1)), polynom::polynomial(0)
))

px <- polyMgen.d(3, 3, rawData=ch2pn(
    c("-3 + z^2", "2 + 4*z", "-z^2", "1", "2", "3 + z", "2*z", "0", "2 - 3*z"), "z"
  )
)
class(px) # "polyMdlist" "polyMatrix"
px

px <- polyMgen.d(3, 3, rawData=ch2pn(
    c("-3 + z^2", "2 + 4*z", "-z^2", "1", "2", "3 + z", "2*z", "0", "2 - 3*z"), "z"
  ),
  byrow=TRUE
)
class(px) # "polyMdlist" "polyMatrix"
px

# clean up
# rm(px)
}

