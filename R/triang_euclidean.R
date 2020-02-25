lead_coef <- function(p)
{
  stopifnot(polynom::is.polynomial(p))
  return(tail(p, 1))
}

is_polynomial_monic <- function(p)
{
  #' Monic polynomial - the leading coefficient (the nonzero coefficient of highest degree) is equal to 1
  return(lead_coef(p) == 1)
}

is_polynomial_zero <- function(p)
{
  return(is.zero(p))
}

is_any_polynomial_nonzero <- function(pl)
{
  stopifnot(all(sapply(pl, polynom::is.polynomial)))
  return(any(sapply(pl, function(p) {!is.zero(p)})))
}

get_min_degree_non_zero_idx <- function(pl, after)
{
  #' Get index of nonzero polynomial in list with minial degree.
  #' In case of mutliply result method returns first index
  stopifnot(all(sapply(pl, polynom::is.polynomial)))

  non_zero_ind <- !sapply(pl, is_polynomial_zero)
  non_zero_pl <- pl[non_zero_ind]
  non_zero_idx <- (1:length(pl))[non_zero_ind]
  if (length(non_zero_pl) == 0) {
    stop("Hermit form doesn't exist")
  }
  p_degree <- sapply(non_zero_pl, degree)
  return(non_zero_idx[which.min(p_degree)])
}

get <- function(pm, row, column)
{
  return(get_row(pm, row)[[column]])
}

get_row <- function(pm, row)
{
  return(pm$dlist[[row]])
}

get_column <- function(pm, column)
{
  return(t(pm)$dlist[[column]])
}

exchange_row <- function(x, first_row, second_row) {
  stopifnot(is.polyMatrix.polyMdlist(x))
  if (first_row == second_row) {
    return(x)
  }
  x$degree[c(first_row, second_row)] <- x$degree[c(second_row, first_row)]
  x$dlist[c(first_row, second_row)] <- x$dlist[c(second_row, first_row)]
  return(x)
}

transf_init <- function(pm)
{
  return(list(m=pm, u=pMdiag(1, nrow(pm), symb=pm$symb)))
}

transf_exchange_row <- function(transf, first_row, second_row)
{
  transf$m <- exchange_row(transf$m, first_row, second_row)
  this_u <- exchange_row(pMdiag(1, nrow(transf$m)), first_row, second_row)
  transf$u <- this_u %X% transf$u
  return(transf)
}

transf_mult_row <- function(transf, row_idx, mult)
{
  transf$m$dlist[[row_idx]] <- lapply(transf$m$dlist[[row_idx]], function(p) {p * mult})
  this_u <- pMdiag(1, nrow(transf$m))
  this_u$dlist[[row_idx]][[row_idx]] <- mult
  transf$u <- this_u %X% transf$u
  return(transf)
}

transf_sub_row <- function(transf, dst_row_idx, src_row_idx, mult=1)
{
  #' substruct from string dst_row_idx row src_row_idx multiplied by mult
  this_u <- pMdiag(1, nrow(transf$m))
  this_u$dlist[[dst_row_idx]][[src_row_idx]] <- -mult
  dst <- transf$m$dlist[[dst_row_idx]]
  src <- transf$m$dlist[[src_row_idx]]
  for(i in 1:ncol(transf$m)) {
    dst[[i]] <- dst[[i]] - mult * src[[i]]
  }
  transf$m$dlist[[dst_row_idx]] <- dst
  transf$u <- this_u %X% transf$u
  return(transf)
}

triang_Euclidean_step <- function(transf, column_idx)
{
  count_limit <- 100 * nrow(transf$m)
  # we can exchange elements starts from row column_idx
  column <- get_column(transf$m, column_idx)
  while(
    !is_polynomial_monic(column[[column_idx]])
    || (column_idx < length(column) && is_any_polynomial_nonzero(column[(column_idx + 1):length(column)]))
  ) {
    # look for min degree row
    min_degree_idx <- get_min_degree_non_zero_idx(column[column_idx:length(column)]) + column_idx - 1
    min_degree_p <- column[[min_degree_idx]]
    # convert to monic
    lead_c <- lead_coef(min_degree_p)
    transf <- transf_mult_row(transf, min_degree_idx, 1 / lead_c)
    # put this element to diagonal
    transf <- transf_exchange_row(transf, min_degree_idx, column_idx)
    # clean all row
    column <- get_column(transf$m, column_idx)
    selected_p <- column[[column_idx]]
    for(r in 1:nrow(transf$m)) {
      # for all other rows we have to remove higher degrees
      if (r != column_idx) {
        coef <- column[[r]] / selected_p
        if (!is_polynomial_zero(coef)) {
          transf <- transf_sub_row(transf, r, column_idx, coef)
        }
      }
    }
    transf$m <- zero_round(transf$m)
    column <- get_column(transf$m, column_idx)
    count_limit <- count_limit - 1
    if (count_limit <= 0) {
      stop("Looks like we get infinite loop in eclidean")
    }
  }
  return(transf)
}

triang_Euclidean <- function(pm)
{
  #' return list of result matrix and u-matrix
  pm <- zero_round(polyMconvert.dlist(pm))
  transf <- transf_init(pm)
  for(c in 1:min(ncol(pm), nrow(pm))) {
    transf <- triang_Euclidean_step(transf, c)
  }
  transf$m <- rebuild_degree(transf$m)
  transf$u <- rebuild_degree(transf$u)
  transf$u$symb <- transf$m$symb
  return(list(T=transf$m, U=transf$u))
}
