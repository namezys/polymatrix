context("coefs")

p <- polynom::polynomial

test_that("degree.polynomial", {
  p <- polynom::polynomial(c(1, 3, 4, 5, 6))
  expect_equal(degree.polynomial(p), 4)
  expect_equal(degree(p), 4)
})


test_that("degree.polyMatrix", {
  pm <- polyMgen.a(degree=c(1, 2, 3, 2, 1, 0))
  expect_equal(degree.polyMatrix(pm), 3)
  expect_equal(degree(pm), 3)
})


test_that("degree_matrix", {
  pm <- polyMgen.a(degree=c(1, 2, 3, 2, 1, 0))
  expect_equal(degree_matrix(pm), matrix(c(1, 2, 3, 2, 1, 0), 2, 3))
})

test_that("degree_column", {
  pm <- polyMgen.a(degree=c(1, 2, 3, 2, 1, 0))
  expect_equal(degree_column(pm), c(2, 3, 1))
})

test_that("degree_row", {
  pm <- polyMgen.a(degree=c(1, 2, 3, 2, 1, 0))
  expect_equal(degree_row(pm), c(3, 2))
})
