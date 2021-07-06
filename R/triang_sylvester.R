.build_sylvester_sub_matrices <- function(pm) {
  stopifnot(is.polyMatrix(pm))
  sub_matrices <- vector("list", nrow(pm))
  sub_nrow <- degree(pm) + 1
  sub_ncol <- ncol(pm)
  for(r in seq_len(nrow(pm))) {
    local <- matrix(0, sub_nrow, sub_ncol)
    for(c in seq_len(ncol(pm))) {
      column <- rev(as.numeric(pm[r, c]))
      local[(sub_nrow - length(column) + 1):sub_nrow, c] <- column
    }
    sub_matrices[[r]] <- local
  }
  return(list(
    sub_matrices=sub_matrices,
    sub_nrow=sub_nrow,
    sub_ncol=sub_ncol
  ))
}

.build_sylvester_matrix <- function(pm, u)
{
  sub_result <- .build_sylvester_sub_matrices(pm)
  sub_nrow <- sub_result$sub_nrow + u
  sub_ncol <- ncol(pm)
  nrow <- sub_nrow * length(sub_result$sub_matrices)
  ncol <- sub_ncol * (u + 1)
  result <- matrix(0, nrow, ncol)
  for(i in 0:u) {
    for(hr in seq_along(sub_result$sub_matrices)) {
      row <- (hr - 1) * sub_nrow + i + 1
      column <- i * sub_ncol + 1
      l_row <- row + sub_result$sub_nrow - 1
      l_column <- column + sub_ncol - 1
      result[row:l_row, column:l_column] <- sub_result$sub_matrices[[hr]]
    }
  }
  return(result)
}

lq <- function(X)
{
  w <- qr(t(X))
  U <- qr.Q(w)
  L <- t(qr.R(w))[sort.list(w$pivot),]
  return(list(L=L,U=U))
}

.extend_for_sylvester <- function(pm)
{
  stopifnot(nrow(pm) < ncol(pm))
  stopifnot(is.polyMatrix(pm))
  return(rbind(pm, diag(polynom::polynomial(1), ncol(pm), ncol(pm))))
}

.shrink_extended_for_sylvester <- function(pm)
{
  stopifnot(nrow(pm) > ncol(pm))
  stopifnot(is.polyMatrix(pm))
  new_nrow <- nrow(pm) - ncol(pm)
  return(pm[1:new_nrow, ])
}

#' Triangularization of a polynomial matrix by Sylvester method
#'
#' The function `triang_Sylvester` triangularize the given polynomial matrix.
#'
#' The `u` parameter is a necessary supplementary input without default value.
#' This parameter give the minimal degree of the searched triangulizator to solve the problem.
#'
#' @param pm an polynomial matrix to triangularize
#' @param u the minimal degree of the triangularizator multiplicator
#' @param eps threshold of non zero coefficients
#'
#' @return T - the left-lower triangularized version of the given polynomial matrix
#' U - the right multiplicator to triangularize the given polynomial matrix
#'
#' @details
#' In a polynomial matrix the head elements are the first non-zero polynomials of columns.
#' The sequence of row indices of this head elements form the \emph{shape} of the polynomial matrix.
#' A polynomial matrix is in left-lower triangular form, if this sequence is monoton increasing.
#'
#' This method search a solution of the triangulrization by the method of Sylvester matrix,
#' descripted in the article Labhalla-Lombardi-Marlin (1996).
#'
#' @references
#'   Salah Labhalla, Henri Lombardi, Roger Marlin:
#'   Algorithm de calcule de la reduction de Hermite d'une matrice a coefficients polynomiaux,
#'   Theoretical Computer Science 161 (1996) pp 69-92
#'
#' @export
triang_Sylvester <- function(pm, u, eps=ZERO_EPS)
{
  if (nrow(pm) < ncol(pm)) {
    was_extended <- TRUE
    pm <- .extend_for_sylvester(pm)
  } else {
    was_extended <- FALSE
  }
  sylv_m <- .build_sylvester_matrix(pm, u)
  lq_result <- lq(sylv_m)
  T <- lq_result$L
  U <- lq_result$U
  lead_hyp_rows <- zero_lead_hyp_rows(T, nrow(T) / nrow(pm), eps)
  if (is.null(lead_hyp_rows)) {
    stop("The given matrix is singular !")
  }

  if (length(unique(lead_hyp_rows)) != ncol(pm)) {
    return(NULL);
  }

  # select columns
  columns <- c(diff(lead_hyp_rows) != 0, TRUE)
  stopifnot(length(columns) == ncol(T))
  SU <- U[, columns]
  ST <- T[, columns]

  # build matrix
  sub_size <- nrow(T) / nrow(pm)
  L <- polyMatrix(0, nrow(pm), ncol(pm), degree(pm))
  for(r in seq_len(nrow(pm))) {
    sub_row <- (r - 1) * sub_size + 1
    sub_row_last <- r * sub_size
    for(c in seq_len(ncol(pm))) {
      L[r, c] <- polynom::polynomial(ST[sub_row_last:sub_row, c])
    }
  }
  T <- polyMatrix(0, ncol(pm), ncol(pm))
  for(r in seq_len(ncol(pm))) {
    u_rows <- (u:0) * ncol(pm) + r
    for(c in seq_len(ncol(pm))) {
      T[r, c] <- polynom::polynomial(SU[u_rows, c])
    }
  }

  if (was_extended) {
    T <- .shrink_extended_for_sylvester(T)
  }
  return(list(L=L, T=T))
}
