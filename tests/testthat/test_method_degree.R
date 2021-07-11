context("method degree")

test_that("invalid request", {
  expect_error(degree("foo"))
  expect_error(degree(TRUE))
  expect_error(degree(c(1, 2)))
})

test_that("degree of number", {
  expect_equal(degree(1), 0)
})

test_that("degree of matrix", {
  expect_equal(degree(matrix(1:6, 2, 3)), 0)
})

test_that("degree of polynomial", {
  expect_equal(degree(p(0)), 0)
  expect_equal(degree(p(c(1, 2, 3))), 2)
  expect_equal(degree(p(0:1000)), 1000)
})

test_that("degree of polyMatrix", {
  expect_equal(degree(polyMatrix(0, 4, 5, 1000)), 0)
  expect_equal(degree(polyMatrix(1, 2, 5, 5)), 5)
})

test_that("degree of char polynomial of polyMatrix", {
  expect_equal(degree(pmcp), 2)
})

test_that("matrix.degree", {
  pm <- parse.polyMatrix("x, 1, x^2",
                         "2x, 0, x^3")
  expect_equal(matrix.degree(pm), matrix(c(1, 0, 2, 1, 0, 3), 2, 3, byrow = TRUE))
})
