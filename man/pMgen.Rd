\name{pMgen}
\alias{pMgen}
\title{Generate a pMatrix class polynomial matrix}
\description{A primitive tool to generate a polynomial matrix in 'pMatrix' form.
             The 'pMatrix' objects independently on the storage method has three 
             invariant element: the \code{$dim}, \code{$degree} and \code{$symb}.
             The \code{$dim} is the size of the matrix. 
             The \code{$degree} is a matrix of non-negetive numbers, the degree of the
             polynomial elements of the polynomial matrix. 
             The \code{$symb} the symbol to print and identify the reference to
             the algebraic set over which the polynom defined -- sorry, this option 
             not fully works.}
\usage{pMgen(k,j,rawData,symb,rand,degree,byrow,sm="pMarray",...)}

\arguments{
  \item{k}{the desired number of rows.}
  \item{j}{the desired number of columns.}
  \item{degree}{the desired degrees of polynomials when it is random filled.}
  \item{rawData}{material to fill the polynomial matrix. 
                In case of \code{pMgen.a}, \code{pMgen.b} and \code{pMgen.c}
                a vector of the required coefficients of the polynomials.
                The case of \code{pMgen.d} is different: here the \code{rawData} is 
                a list of \code{\link{polynomial}} class objects.}
  \item{symb}{the desired polynom symbol.}
  \item{rand}{the name of wanted random generator to generate the
              coefficients of a random polynomial matrix.}        
  \item{byrow}{logical. If \code{FALSE} (the default) the matrix is filled by columns, if \code{TRUE} the matrix is filled by rows.}        
  \item{sm}{storage method: \code{pMarray} (the default) or \code{pMbroad} \code{pMcells} \code{pMdlist} }        
  \item{\dots}{...}        
}

\details{A polynomial matrix from the given material \code{rawData} or
         (the default) or random values
the four possible storage methods are:
\itemize{
\item 'array', when the coefficient matrices of the polinomial matrix stored in two
      independent element of the list of the \code{pMatrix}: the constant matrix in 
      \code{$const}, and the coefficients of the first, second... degree of the polynomial
      in the first, second,... element of the \code{$array}.
      In this case the extra class of the generated \code{pMatrix} object is \code{pMarray}.
\item 'broad-matrix', when the coefficients are stored in one broad matrix in the \code{$broad}
      element of the \code{pMatrix} object. The size of this broad matrix is: \code{nx(m*(k+1))}, when the degree of the \code{nxm} size polynomial matrix is \code{d}.
      In this case the extra class of the generated \code{pMatrix} object is \code{pMbroad}.
\item 'list of cells', when the \code{nxm} size coefficient matrices are stored 
      in \code{d+1} element of the \code{$cells} list element of the \code{pMatrix} object. 
      In this case the extra class of the generated \code{pMatrix} object is \code{pMcells}.
\item 'double list', when the elements of the polynomial matrix are stored in the form
      of \code{polynomial} class objects in a \code{nxm} element double list.
      In this case the extra class of the generated \code{pMatrix} object is \code{pMdlist}.
}
         
         }
\value{A \code{pMatrix} class object with a extra class \code{pMarray}, \code{pMbroad}, \code{pMcells} or \code{pMdlist}.}
\seealso{\code{\link{pMatrix-package}}, 
         \code{\link{pMgen.a}}, \code{\link{pMgen.b}}, \code{\link{pMgen.c}}, \code{\link{pMgen.d}}}

\examples{

pMgen.a() # default size & coefficients & rank

pMgen.a(2,3,1:6) # given coefficients & default rank
pMgen.a(2,3,1:6,degree=1) # given coefficients & rank
pMgen.a(2,3,1:36,degree=matrix(0:5,2,3)) # given coefficients & ranks
pMgen.a(2,3,1:12,degree=matrix(2:0,2,3)) # given coefficients & ranks

pMgen.a(rand=TRUE) # normally distributed coefficients

pMgen.a(rand=rexp) # exponentially distributed coefficients

pois.vg.fv<-function(x) rpois(x,1)
pMgen.a(rand=pois.vg.fv) # Poisson(1) distributed coefficients
rm(pois.vg.fv)

pMgen.b() # class="pMbroad", broad matrix form
pMgen.c() # class="pMcells", list of coefficient matrices
pMgen.d() # class="pMdlist", dubble list 

pMgen.d(2,2,
  rawData=list(polynom::polynomial(1),
               polynom::polynomial(c(0,0,1)),
               polynom::polynomial(c(0,1)),
               polynom::polynomial(0)))

px<-pMgen.d(3,3,rawData=ch2pn(
     c("-3 + z^2","2 + 4*z","-z^2",
       "1","2","3 + z",
       "2*z","0","2 - 3*z"),"z"))
class(px) # "pMdlist" "pMatrix"
px

px<-pMgen.d(3,3,rawData=ch2pn(
     c("-3 + z^2","2 + 4*z","-z^2",
       "1","2","3 + z",
       "2*z","0","2 - 3*z"),"z"),byrow=TRUE)
class(px) # "pMdlist" "pMatrix"
px

pMgen.d()
pMgen.d(rand=TRUE)
pMgen.d(rand=TRUE,degree=2)
pMgen.d(degree=3)
pMgen.d(degree=NULL)


}
\keyword{datagen}

