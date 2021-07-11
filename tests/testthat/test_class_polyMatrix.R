context("class polyMatrix")


test_that("create from numerica", {
  p <- polyMatrix(1, 2, 3, 1)
  expect_equal(p@coef, matrix(1, 2, 3 * 2))
})

test_that("create from sequence", {
  result <- matrix(
    c(1, 3, 5, 1, 3, 5,
      2, 4, 6, 2, 4, 6),
    2, 6, byrow = TRUE
  )
  p <- polyMatrix(1:6, 2, 3, 1)
  expect_equal(p@coef, result)
  expect_equal(p@ncol, 3)
})

test_that("create form matrix full size", {
  result <- cbind(matrix(0, 2, 4), matrix(1, 2, 4))
  p <- polyMatrix(result, 2, 4, 1)
  expect_equal(p@coef, result)
  expect_equal(p@ncol, 4)
})

test_that("create from matrix less size", {
  data <- matrix(
    c(1, 3,
      2, 5),
    2, 2, byrow = TRUE
  )
  result <- cbind(data, data, data)
  p <- polyMatrix(data, 2, 3, 1)
  expect_equal(p@coef, result)
  expect_equal(p@ncol, 3)
})

test_that("create from polynomial", {
  pp <- p(1, 2, 3)
  pm <- polyMatrix(pp)
  expect_equal(pm@coef, matrix(c(1, 2, 3), 1, 3))
  expect_equal(pm@ncol, 1)

  pm_b <- polyMatrix(pp, 3, 2, 10)
  expect_equal(pm_b@coef, matrix(c(
    1, 1, 2, 2, 3, 3,
    1, 1, 2, 2, 3, 3,
    1, 1, 2, 2, 3, 3
  ), 3, 6, byrow = TRUE))
  expect_equal(pm_b@ncol, 2)
})

# check type validation

test_that("type validation", {
  expect_error(polyMatrix(1, 1, ))
  expect_error(polyMatrix(1, , 1))
  expect_error(polyMatrix(, 1, 1))

  expect_error(polyMatrix(1, -1, 1))
  expect_error(polyMatrix(1, 1, -1))
  expect_error(polyMatrix(1, 1, 1, -1))


  expect_error(polyMatrix(NULL, 1, 1))
  expect_error(polyMatrix("", 1, 1))
  expect_error(polyMatrix("0", 1, 1))
  expect_error(polyMatrix(TRUE, 1, 1))
})

# reduce degree

test_that("reduce degree: zero", {
  expect_equal(dim(polyMatrix(0, 2, 3, 100)@coef), c(2, 3))
})

test_that("reduce degree: two full zero and one full zero", {
  data <- cbind(matrix(1, 2, 6), matrix(0, 2, 3))
  p <- polyMatrix(data, 2, 3, 2)
  expect_equal(dim(p@coef), c(2, 6))
})

test_that("reduce degree: all zero except one", {
  data <- matrix(0, 2, 9)
  data[2, 5] <- 1
  p <- polyMatrix(data, 2, 3, 2)
  expect_equal(dim(p@coef), c(2, 6))
})