\name{polyMgen}
\alias{polyMgen}
\title{Generate a polyMatrix class polynomial matrix}
\description{
A complex tool to generate a polynomial matrix in \sQuote{polyMatrix} form.
The \sQuote{polyMatrix} object contains the following three
invariant elements: the \code{$dim}, \code{$degree} and \code{$symb}
independently of the sub-class of the object.
The \code{$dim} is the size of the matrix.
The \code{$degree} is a matrix of non-negetive numbers, the degree of the
polynomial elements of the polynomial matrix.
The \code{$symb} the symbol to print and identify the reference to
the algebraic set over which the polynom defined -- sorry, this option
not fully works.}

\usage{polyMgen(nrow,ncol,rawData,symb,rand,degree,byrow,sm="polyMarray",...)}
\arguments{
  \item{nrow}{the desired number of rows}
  \item{ncol}{the desired number of columns}
  \item{degree}{the desired degrees of polynomials when it is filled randomly}
  \item{rawData}{the data to fill with the polynomial matrix.
                In case of \code{polyMgen.a}, \code{polyMgen.b} and \code{polyMgen.c}
                a vector of the required coefficients of the polynomials.
                The case of \code{polyMgen.d} is different: here the \code{rawData} is
                a list of \code{\link{polynomial}} class objects}
  \item{symb}{the desired polynom symbol}
  \item{rand}{the name of the random generator used to generate the
              coefficients of a random polynomial matrix}
  \item{byrow}{logical. If \code{FALSE} (the default) the matrix is filled by columns, if \code{TRUE} the matrix is filled by rows}
  \item{sm}{storage method: \code{polyMarray} (the default) or\\
           \code{polyMbroad}, \code{polyMcells}, \code{polyMdlist} }
  \item{\dots}{additional arguments}
}

\details{
The four possible methods to generate the polynomial matrix from the given material \code{rawData} or (the default) or random values:
\itemize{
\item \sQuote{array}, when the coefficient matrices of the polinomial matrix are stored in two
      independent element of the list of the \code{polyMatrix}: the constant matrix in
      \code{$const}, and the coefficients of the first, second... degree of the polynomial
      in the first, second,... element of the \code{$array}.
      In this case the sub-class of the generated \code{polyMatrix} object is \code{polyMarray}.
\item \sQuote{broad-matrix},
      when the coefficients are stored in one broad matrix in the \code{$broad}
      element of the \code{polyMatrix} object.
	  The size of this broad matrix is: \code{nrow x (ncol*(d+1))},
	  when the degree of the \code{nrow x ncol} size polynomial matrix is \code{d}.
      In this case the sub-class of the generated \code{polyMatrix} object is \code{polyMbroad}.
\item \sQuote{list of cells}, when the \code{nrow x ncol} size coefficient matrices are stored
      in \code{d+1} element of the \code{$cells} list element of the \code{polyMatrix} object.
      In this case the sub-class of the generated \code{polyMatrix} object is \code{polyMcells}.
\item \sQuote{double list}, when the elements of the polynomial matrix are stored in the form
      of \code{polynomial} class objects in a \code{nrow x ncol} element double list.
      In this case the sub-class of the generated \code{polyMatrix} object is \code{polyMdlist}.
}

         }
\value{A \code{polyMatrix} class object with a sub-class \code{polyMarray}, \code{polyMbroad}, \code{polyMcells} or \code{polyMdlist}.}
\seealso{\code{\link{polyMatrix-package}},
         \code{\link{polyMgen.a}}, \code{\link{polyMgen.b}}, \code{\link{polyMgen.c}}, \code{\link{polyMgen.d}}}

\examples{

polyMgen.a() # default size & coefficients & rank

polyMgen.a(2,3,1:6) # given coefficients & default rank
polyMgen.a(2,3,1:6,degree=1) # given coefficients & rank
polyMgen.a(2,3,1:36,degree=matrix(0:5,2,3)) # given coefficients & ranks
polyMgen.a(2,3,1:12,degree=matrix(2:0,2,3)) # given coefficients & ranks

polyMgen.a(rand=TRUE) # normally distributed coefficients

polyMgen.a(rand=rexp) # exponentially distributed coefficients

pois.vg.fv <- function(x) rpois(x,1)
polyMgen.a(rand=pois.vg.fv) # Poisson(1) distributed coefficients
rm(pois.vg.fv)

polyMgen.b() # class="polyMbroad", broad matrix form
polyMgen.c() # class="polyMcells", list of coefficient matrices
polyMgen.d() # class="polyMdlist", dubble list

polyMgen.d(2,2,
  rawData=list(polynom::polynomial(1),
               polynom::polynomial(c(0,0,1)),
               polynom::polynomial(c(0,1)),
               polynom::polynomial(0)))

px <- polyMgen.d(3,3,rawData=ch2pn(
     c("-3 + z^2","2 + 4*z","-z^2",
       "1","2","3 + z",
       "2*z","0","2 - 3*z"),"z"))
class(px) # "polyMdlist" "polyMatrix"
px

px <- polyMgen.d(3,3,rawData=ch2pn(
     c("-3 + z^2","2 + 4*z","-z^2",
       "1","2","3 + z",
       "2*z","0","2 - 3*z"),"z"),byrow=TRUE)
class(px) # "polyMdlist" "polyMatrix"
px

polyMgen.d()
polyMgen.d(rand=TRUE)
polyMgen.d(rand=TRUE,degree=2)
polyMgen.d(degree=3)
polyMgen.d(degree=NULL)

# clean up
# rm(px)

}
\keyword{datagen}

