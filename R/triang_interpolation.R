newton <- function(C, points)
{
  #' Build matrix of polynimal decomposition using Newton interpolation in Newton bais:
  #' (x-x_0), (x - x_0) * (x x_1)
  #'
  #' @param C Matrix of values of polinomials in columns
  #' @param points point in which the values of polynomials were got
  #'
  #' @return Matrix of coefficients in columns (from higher degree to lower)
  d <- length(points) - 1
  stopifnot(nrow(C) == d + 1)
  result <- C
  for(k in 1:d) {
    for(i in 1:(d + 1 - k)) {
      result[i,] <- (result[i,] - result[i + 1,]) / (points[d + 1 - i + 1] - points[d + 1 - i + 1 - k])
    }
  }
  return(result)
}

triang_Interpolation <- function(pm, point_vector, round_digits=5, eps=.Machine$double.eps ^ 0.5)
{
  #' Triangularization of a polynomial matrix by interpolation method
  #'
  #' The parameters `point_vector`, `round_digits` can significantly affect the result.
  #'
  #' Default value of `eps`` usually is enought to determintate real zeros.
  #'
  #' In a polynomial matrix the head elements are the first non-zero polynomials of columns.
  #' The sequence of row indices of this head elements form the shape of the polynomial matrix.
  #' A polynomial matrix is in left-lower triangular form, if this sequence is monoton increasing.
  #'
  #' This method offers a solution of the triangulrization by the Interpolation method,
  #' described in the article of Labhalla-Lombardi-Marlin (1996).
  #'
  #' @param pm source polynimial matrix
  #' @param point_vector vector of interpolation points
  #' @param round_digits we will try to round result on each step
  #' @param eps calculation zero errors
  #' @return Tranfortmaiton matrix
  #'
  # @examples
  # A <- polyMgen.d(3,2,ch2pn(c("x-1","2","0","x^2-1","2*x+2","3")))
  #
  # triang_Interpolation(A, -2:2)
  # # 0.79057 - 0.31623*x + 0.15812*x^2   -0.57735 - 0.57735*x
  # # 0.47434 - 0.15811*x - 1e-05*x^2     0.57735
  #
  # triang_Interpolation(A, -10:10)
  # # 0.79057 - 0.3161*x + 0.15803*x^2   0.25574 - 0.3541*x - 0.60984*x^2
  # # 0.47448 - 0.15807*x                -0.25574 + 0.60984*x


  # numerical matrix of values should contains enough rows for store all values from point_vector
  # and enough columns to store all degrees
  degree <- degree(pm)
  point_number <- length(point_vector)
  hyp_nrow <- nrow(pm)
  hyp_ncol <- ncol(pm)

  # define numerical matrix with submatrices (each one contains all columnt from source matrix and one row per each point)
  numeric_matrix <- matrix(NA, hyp_nrow * point_number, hyp_ncol * (degree + 1))

  # build numerical matrix
  for(hyp_row in 1:hyp_nrow) {
    hyp_row_polynomials <- pm[hyp_row, ]
    for(sub_row in 1:point_number) {
      point <- point_vector[sub_row]
      sub_values <- sapply(hyp_row_polynomials, function(p) predict(p, point))
      row_values <- rep(sub_values, degree + 1)
      row_mult <- rep(point ^ (degree:0), each=hyp_ncol)
      row <- (hyp_row - 1) * point_number + sub_row
      numeric_matrix[row, ] <- row_values * row_mult
    }
  }

  # LQ-decomposition of numerical matrix
  lq_dec <- lq(numeric_matrix)
  U <- lq_dec$U
  T <- lq_dec$L
  # after LQ decompostion we can determminate which hyper elemement will be converted to zero polynomail:
  # if all elements in column of hyperrow are zero, it will results in zero polinomial after interpolation

  # the first non zeroes idx
  lead_hyp_rows <- zero_lead_hyp_rows(T, point_number, eps)

  stopifnot(length(unique(lead_hyp_rows)) == hyp_ncol)
  # collect columns to matrix Uv
  Uv <- matrix(NA, (degree + 1) * hyp_ncol, hyp_ncol)
  for(col in 1:hyp_ncol) {
    # try to get hyp column
    hyp_row = unique(lead_hyp_rows)[col]
    S_T <- round(T[(point_number * (hyp_row - 1) + 1):(point_number * hyp_row), lead_hyp_rows==hyp_row], round_digits)
    # newton coeffieicents allow to find matrix with lowest degree
    newton_coef <- round(newton(S_T, point_vector), round_digits)
    # use LQ decomposition to reduce calculation errors
    selected_columns <- round(lq(newton_coef)$U[, ncol(S_T)], round_digits)
    # select column from matrix U
    Uv[, col] <- round(U[,lead_hyp_rows==hyp_row] %*% selected_columns, round_digits)
  }

  # select polynomial coef from Uv
  L <- polyMatrix(0, hyp_nrow, hyp_ncol)
  for(row in 1:hyp_ncol) {
    for(col in 1:hyp_ncol) {
      source_rows <- ((degree):0) * hyp_ncol + row
      L[row, col] <- polynom::polynomial(Uv[source_rows, col])
    }
  }
  return(L)
}
