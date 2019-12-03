\name{triang_Sylvester}
\alias{triang_Sylvester}
\title{Triangularization of a polynomial matrix by Sylvester method}

\description{
  The function \code{triang_Sylvester} triangularize the given polynomial matrix.

  The \code{u} parameter is a necessary supplementary input without default value.
  This parameter give the minimal degree of the searched triangulizator to solve the problem.
}

\usage{
  triang_Sylvester(pm,u, eps=ZERO_EPS)
}

\arguments{
  \item{pm}{polynomial matrix to triangularize}
  \item{u}{the minimal degree of the triangularizator multiplicator}
  \item{eps}{toleranz limit}
}

\details{
  In a polynomial matrix the head elements are the first non-zero polynomials of columns.
  The sequence of row indices of this head elements form the \emph{shape} of the polynomial matrix.
  A polynomial matrix is in left-lower triangular form, if this sequence is monoton increasing.

  This method search a solution of the triangulrization by the method of Sylvester matrix,
  descripted in the article Labhalla-Lombardi-Marlin (1996).
}

\value{
  \item{T}{the left-lower triangularized version of the given polynomial matrix}
  \item{U}{the right multiplicator to triangularize the given polynomial matrix}
}

\references{
  Salah Labhalla, Henri Lombardi, Roger Marlin:
  Algorithm de calcule de la reduction de Hermite d'une matrice a coefficients polynomiaux,
  Theoretical Computer Science 161 (1996) pp 69-92
}

\author{Nikolai Ryzhkov, \email{namezys@gmail.com}}

\seealso{\code{\link{triang_Euclidean}}}

\keyword{triangularization}
