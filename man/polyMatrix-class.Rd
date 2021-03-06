% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_polyMatrix.R, R/method_coef.R,
%   R/method_det.R, R/method_dim.R, R/method_predict.R, R/method_round.R,
%   R/method_show.R, R/method_subscript.R, R/ops_addition.R, R/ops_equal.R,
%   R/ops_matrix_mult.R, R/ops_multiplication.R, R/ops_substraction.R
\docType{class}
\name{polyMatrix-class}
\alias{polyMatrix-class}
\alias{polyMatrixClass}
\alias{[[,polyMatrix,numeric-method}
\alias{det,polyMatrix-method}
\alias{nrow,polyMatrix-method}
\alias{nrow,polynomial-method}
\alias{ncol,polyMatrix-method}
\alias{ncol,polynomial-method}
\alias{dim,polyMatrix-method}
\alias{predict,polyMatrix-method}
\alias{round,polyMatrix-method}
\alias{show,polyMatrix-method}
\alias{[,polyMatrix,missing,missing,missing-method}
\alias{[,polyMatrix,missing,ANY,missing-method}
\alias{[,polyMatrix,ANY,missing,missing-method}
\alias{[,polyMatrix,logical,logical,missing-method}
\alias{[,polyMatrix,logical,numeric,missing-method}
\alias{[,polyMatrix,numeric,logical,missing-method}
\alias{[,polyMatrix,numeric,numeric,missing-method}
\alias{[<-,polyMatrix,missing,missing,ANY-method}
\alias{[<-,polyMatrix,missing,ANY,ANY-method}
\alias{[<-,polyMatrix,ANY,missing,ANY-method}
\alias{[<-,polyMatrix,numeric,numeric,numeric-method}
\alias{[<-,polyMatrix,numeric,numeric,matrix-method}
\alias{[<-,polyMatrix,numeric,numeric,polynomial-method}
\alias{[<-,polyMatrix,numeric,numeric,polyMatrix-method}
\alias{+,polyMatrix,missing-method}
\alias{+,polyMatrix,polyMatrix-method}
\alias{+,polyMatrix,polynomial-method}
\alias{+,polyMatrix,numeric-method}
\alias{+,polyMatrix,matrix-method}
\alias{+,ANY,polyMatrix-method}
\alias{==,polyMatrix,polyMatrix-method}
\alias{==,polyMatrix,polynomial-method}
\alias{==,polyMatrix,matrix-method}
\alias{==,polyMatrix,numeric-method}
\alias{==,ANY,polyMatrix-method}
\alias{!=,polyMatrix,ANY-method}
\alias{!=,ANY,polyMatrix-method}
\alias{\%*\%,polyMatrix,polyMatrix-method}
\alias{\%*\%,polyMatrix,matrix-method}
\alias{\%*\%,matrix,polyMatrix-method}
\alias{*,polyMatrix,numeric-method}
\alias{*,polyMatrix,polynomial-method}
\alias{*,polyMatrix,polyMatrix-method}
\alias{*,ANY,polyMatrix-method}
\alias{-,polyMatrix,polyMatrix-method}
\alias{-,polyMatrix,ANY-method}
\alias{-,ANY,polyMatrix-method}
\title{A class to represent matrix of polinomials}
\usage{
\S4method{[[}{polyMatrix,numeric}(x, i)

\S4method{det}{polyMatrix}(x)

\S4method{nrow}{polyMatrix}(x)

\S4method{nrow}{polynomial}(x)

\S4method{ncol}{polyMatrix}(x)

\S4method{ncol}{polynomial}(x)

\S4method{dim}{polyMatrix}(x)

\S4method{predict}{polyMatrix}(object, newdata)

\S4method{round}{polyMatrix}(x, digits = 0)

\S4method{show}{polyMatrix}(object)

\S4method{[}{polyMatrix,missing,missing,missing}(x, i, j, ..., drop = TRUE)

\S4method{[}{polyMatrix,missing,ANY,missing}(x, i, j)

\S4method{[}{polyMatrix,ANY,missing,missing}(x, i, j)

\S4method{[}{polyMatrix,logical,logical,missing}(x, i, j)

\S4method{[}{polyMatrix,logical,numeric,missing}(x, i, j)

\S4method{[}{polyMatrix,numeric,logical,missing}(x, i, j)

\S4method{[}{polyMatrix,numeric,numeric,missing}(x, i, j)

\S4method{[}{polyMatrix,missing,missing,ANY}(x, i, j) <- value

\S4method{[}{polyMatrix,missing,ANY,ANY}(x, i, j) <- value

\S4method{[}{polyMatrix,ANY,missing,ANY}(x, i, j) <- value

\S4method{[}{polyMatrix,numeric,numeric,numeric}(x, i, j) <- value

\S4method{[}{polyMatrix,numeric,numeric,matrix}(x, i, j) <- value

\S4method{[}{polyMatrix,numeric,numeric,polynomial}(x, i, j) <- value

\S4method{[}{polyMatrix,numeric,numeric,polyMatrix}(x, i, j) <- value

\S4method{+}{polyMatrix,missing}(e1, e2)

\S4method{+}{polyMatrix,polyMatrix}(e1, e2)

\S4method{+}{polyMatrix,polynomial}(e1, e2)

\S4method{+}{polyMatrix,numeric}(e1, e2)

\S4method{+}{polyMatrix,matrix}(e1, e2)

\S4method{+}{ANY,polyMatrix}(e1, e2)

\S4method{==}{polyMatrix,polyMatrix}(e1, e2)

\S4method{==}{polyMatrix,polynomial}(e1, e2)

\S4method{==}{polyMatrix,matrix}(e1, e2)

\S4method{==}{polyMatrix,numeric}(e1, e2)

\S4method{==}{ANY,polyMatrix}(e1, e2)

\S4method{!=}{polyMatrix,ANY}(e1, e2)

\S4method{!=}{ANY,polyMatrix}(e1, e2)

\S4method{\%*\%}{polyMatrix,polyMatrix}(x, y)

\S4method{\%*\%}{polyMatrix,matrix}(x, y)

\S4method{\%*\%}{matrix,polyMatrix}(x, y)

\S4method{*}{polyMatrix,numeric}(e1, e2)

\S4method{*}{polyMatrix,polynomial}(e1, e2)

\S4method{*}{polyMatrix,polyMatrix}(e1, e2)

\S4method{*}{ANY,polyMatrix}(e1, e2)

\S4method{-}{polyMatrix,polyMatrix}(e1, e2)

\S4method{-}{polyMatrix,ANY}(e1, e2)

\S4method{-}{ANY,polyMatrix}(e1, e2)
}
\arguments{
\item{x}{an matrix object}

\item{i}{the degree to extract matrix of coefficient}

\item{object}{an \R object}

\item{newdata}{the value to evaluate}

\item{digits}{integer indicating the number of decimal places (round) or significant digits (signif) to be used}

\item{j}{column indeces}

\item{...}{unused}

\item{drop}{unused}

\item{value}{new value}

\item{e1}{an left operand}

\item{e2}{an right operand}

\item{y}{second argument}
}
\description{
A class to represent matrix of polinomials
}
\section{Methods (by generic)}{
\itemize{
\item \code{[[}: get coefficient matrix by degree

\item \code{det}: determinant of a polynomial matrix

\item \code{nrow}: number of rows of a polynomial matrix

\item \code{nrow}: an polynomial has only one row

\item \code{ncol}: number of column of a polynomial matrix

\item \code{ncol}: an polynomial has only one column

\item \code{dim}: dimension of a polynomial matrix

\item \code{predict}: value of polynomial matrix in point

\item \code{round}: round of polynomial matrix is rounding of polynomial coefficients

\item \code{show}: prints out a text representation of a polynomial matrix

\item \code{[}: get matrix content

\item \code{[}: get columns

\item \code{[}: get rows

\item \code{[}: get by logical index

\item \code{[}: get by logical index and numerical indeces

\item \code{[}: get by logical index and numerical indeces

\item \code{[}: get by row and column indeces

\item \code{[<-}: replace matrix content

\item \code{[<-}: assign rows

\item \code{[<-}: assign columns

\item \code{[<-}: assign part of matrix with number

\item \code{[<-}: assign part of matrix with another matrix

\item \code{[<-}: assign part of matrix with polynomial

\item \code{[<-}: assign part of matrix with another polynomial matrix

\item \code{+}: summation with polynomial matrix

\item \code{+}: summation of polynomial matrices

\item \code{+}: summation of polynomial matrix and scalar polynomial

\item \code{+}: summation of polynomial matrix and scalar nummber

\item \code{+}: summation of polynomial matrix and numerical matrix

\item \code{+}: summation of polynomial matrix

\item \code{==}: equal operator for two polinomial matrices, result is a boolean matrix

\item \code{==}: equal operator for polinomail matrix and polinomail, result is a matrix

\item \code{==}: equal operator for polinomial and numerical matrices

\item \code{==}: equal operator for polinomial matrix and number, result is a matrix

\item \code{==}: equal operator for aby object and polinomial matrix

\item \code{!=}: not equal operator

\item \code{!=}: not equal operator

\item \code{\%*\%}: matrix multiplicatoin of polynomial matrices

\item \code{\%*\%}: matrix multiplicatoin of polynomial and numerical matrices

\item \code{\%*\%}: matrix multiplicatoin of numerical and polynomial matrices

\item \code{*}: scalar multiplication with number

\item \code{*}: scalar multiplication with polynomial

\item \code{*}: scalar multiplication of polynomial mattrices elementwise

\item \code{*}: scalar multiplication

\item \code{-}: substractioin

\item \code{-}: substractioin

\item \code{-}: substractioin
}}

\section{Slots}{

\describe{
\item{\code{coef}}{A matrix of coefficients which are joined into one matrix from lower degree to higher}

\item{\code{ncol}}{Actual number of columns in the polynomial matrix}
}}

\examples{

# create a new polynomial matrix by parsing strings
pm <- parse.polyMatrix(
     "x; 1 + x^2; 3 x - x^2",
     "1; 1 + x^3; - x + x^3"
)

# get coefficient matrix for degree 0
pm[[0]]
##      [,1] [,2] [,3]
## [1,]    0    1    0
## [2 ]    1    1    0
# get coefficient matrix for degree 1
pm[[1]]
##      [,1] [,2] [,3]
## [1,]    1    0    3
## [2 ]    0    0   -1


# dimensions
nrow(pm) ## 2


ncol(pm) ## 3


dim(pm) ## [1] 2 3


# round
round(parse.polyMatrix(
  "      1.0001 - x,            1 - x^2, 1 + 2.0003*x + x^2",
  "0.0001 + x - x^2, 1 + x + 0.0001 x^2, 1 - 2*x + x^2"
))
##           [,1]      [,2]           [,3]
## [1,]     1 - x   1 - x^2   1 + 2x + x^2
## [2,]   x - x^2     1 + x   1 - 2x + x^2


# print out a polynomial matrix
show(parse.polyMatrix(
  "      1.0001 - x,          1 - x^2, 1 + 2.0003*x + x^2",
  "0.0001 + x - x^2,            1 + x, 1 - 2*x + x^2",
  "        12.3 x^3,  2 + 3.5 x + x^4, -0.7 + 1.6e-3 x^3"
))
##                   [,1]             [,2]                [,3]
## [1,]        1.0001 - x          1 - x^2   1 + 2.0003x + x^2
## [2,]   1e-04 + x - x^2            1 + x        1 - 2x + x^2
## [3,]           12.3x^3   2 + 3.5x + x^4    -0.7 + 0.0016x^3

}
