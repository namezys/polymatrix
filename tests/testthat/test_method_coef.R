context("method coefficient")


test_that("get coefficient matrix", {
  # 1 + x + 2x^2    1 + 4x^2    3x
  #   3 x +  x^2      2         0
  c0 <- matrix(c(1, 1, 0, 0, 2, 0), 2, 3, byrow = TRUE)
  c1 <- matrix(c(1, 0, 3, 3, 0, 0), 2, 3, byrow = TRUE)
  c2 <- matrix(c(2, 4, 0, 1, 0, 0), 2, 3, byrow = TRUE)
  p <- polyMatrix(cbind(c0, c1, c2), 2, 3, 2)
  expect_error(p[[-1]])
  expect_equal(p[[0]], c0)
  expect_equal(p[[1]], c1)
  expect_equal(p[[2]], c2)
  expect_error(p[[3]])
})

test_that("get coef of char polynomial of polyMarix", {
  expect_equal(pmcp[[0]], p(1, 1, 0))
  expect_equal(pmcp[[1]], p(0, 1, -1))
  expect_equal(pmcp[[2]], p(-1, 0, 2))
})