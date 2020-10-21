context("methods ncol, nrow, dim")

test_that("nrow", {
  expect_equal(nrow(matrix(1, 6, 4)), 6)
  expect_equal(nrow(polyMatrix(1, 3, 7)), 3)
  expect_equal(nrow(polyMatrix(1, 4, 2, 11)), 4)
  expect_equal(nrow(polyMatrix(0, 2, 9, 11)), 2)

  expect_equal(nrow(p(1, 2, 3, 4)), 1)
})
test_that("ncol", {
  expect_equal(ncol(matrix(1, 6, 4)), 4)
  expect_equal(ncol(polyMatrix(1, 3, 7)), 7)
  expect_equal(ncol(polyMatrix(1, 4, 2, 11)), 2)
  expect_equal(ncol(polyMatrix(0, 2, 9, 11)), 9)

  expect_equal(ncol(p(1, 4, 5, 6)), 1)
})

test_that("dim", {
  expect_equal(dim(matrix(1, 6, 4)), c(6, 4))
  expect_equal(dim(polyMatrix(1, 3, 7)), c(3, 7))
  expect_equal(dim(polyMatrix(1, 4, 2, 11)), c(4, 2))
  expect_equal(dim(polyMatrix(0, 2, 9, 11)), c(2, 9))

  # TODO: expect_equal(dim(p(3, 4, 5, 6, 9)), c(1, 1))
})
