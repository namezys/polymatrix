build_sylvester_sub_matrices <- function(pm)
{
  stopifnot(is.polyMatrix.polyMdlist(pm))
  sub_matrices <- vector("list", nrow(pm))
  sub_nrow <- degree(pm) + 1
  sub_ncol <- ncol(pm)
  for(r in 1:length(sub_matrices)) {
    local <- matrix(0, sub_nrow, sub_ncol)
    row <- pm$dlist[[r]]
    for(c in 1:length(row)) {
      column <- rev(as.numeric(row[[c]]))
      local[(sub_nrow - length(column) + 1):sub_nrow, c] <- column
    }
    sub_matrices[[r]] <- local
  }
  return(list(sub_matrices=sub_matrices, sub_nrow=sub_nrow, sub_ncol=sub_ncol))
}

build_sylvester_matrix <- function(pm, u)
{
  sub_result <- build_sylvester_sub_matrices(pm)
  sub_nrow <- sub_result$sub_nrow + u
  sub_ncol <- ncol(pm)
  nrow <- sub_nrow * length(sub_result$sub_matrices)
  ncol <- sub_ncol * (u + 1)
  result <- matrix(0, nrow, ncol)
  for(i in 0:u) {
    for(hr in 1:length(sub_result$sub_matrices)) {
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

extend_for_sylvester <- function(pm)
{
  stopifnot(nrow(pm) < ncol(pm))
  stopifnot(is.polyMatrix.polyMdlist(pm))
  new_nrow <- nrow(pm) + ncol(pm)
  data <- vector("list", new_nrow * ncol(pm))
  for(r in 1:nrow(pm)) {
    for(c in 1:ncol(pm)) {
      data[[ncol(pm) * (r - 1) + c]] <- pm$dlist[[r]][[c]]
    }
  }
  for(er in 1:ncol(pm)) {
    for(c in 1:ncol(pm)) {
      if (er == c) {
        p <- polynom::polynomial(1)
      } else {
        p <- polynom::polynomial(0)
      }
      data[[ncol(pm) * (nrow(pm) + er - 1) + c]] <- p
    }
  }
  return(polyMgen.d(nrow(pm) + ncol(pm), ncol(pm), rawData=data, symb=pm$symb, byrow=TRUE))
}

shrink_extended_for_sylvester <- function(pm)
{
  stopifnot(nrow(pm) > ncol(pm))
  stopifnot(is.polyMatrix.polyMdlist(pm))
  new_nrow <- nrow(pm) - ncol(pm)
  data <- vector("list", new_nrow * ncol(pm))
  for(r in 1:new_nrow) {
    for(c in 1:ncol(pm)) {
      data[[ncol(pm) * (r - 1) + c]] <- pm$dlist[[r]][[c]]
    }
  }
  return(polyMgen.d(new_nrow, ncol(pm), rawData=data, symb=pm$symb, byrow=TRUE))
}

triang_Sylvester <- function(pm, u, eps=ZERO_EPS)
{
  pm <- polyMconvert.dlist(pm)
  if (nrow(pm) < ncol(pm)) {
    was_extended <- TRUE
    pm <- extend_for_sylvester(pm)
  } else {
    was_extended <- FALSE
  }
  sylv_m <- build_sylvester_matrix(pm, u)
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
  t_list <- vector("list", nrow(pm) * ncol(pm))
  for(r in 1:nrow(pm)) {
    sub_row <- (r - 1) * sub_size + 1
    sub_row_last <- r * sub_size
    for(c in 1:ncol(pm)) {
      t_list[[(c - 1) * nrow(pm) + r]] <- polynom::polynomial(ST[sub_row_last:sub_row, c])
    }
  }
  u_list <- vector("list", ncol(pm) * ncol(pm))
  for(r in 1:ncol(pm)) {
    u_rows <- (u:0) * ncol(pm) + r
    for(c in 1:ncol(pm)) {
      u_list[[(c - 1) * ncol(pm) + r]] <- polynom::polynomial(SU[u_rows, c])
    }
  }

  RT <- polyMgen.d(nrow(pm), ncol(pm), rawData=t_list, symb=pm$symb)
  if (was_extended) {
    RT <- shrink_extended_for_sylvester(RT)
  }
  RU <- polyMgen.d(ncol(pm), ncol(pm), rawData=u_list, symb=pm$symb)
  return(list(T=RT, U=RU))
}
