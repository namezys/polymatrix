context("coefs")

p <- polynom::polynomial

test_that("coefs.polynomial", {
  p <- polynom::polynomial(c(1, 3, 4, 5, 6))
  expect_equal(coefs.polynomial(p), c(1, 3, 4, 5, 6))
  expect_equal(coefs.polynomial(p, 0), 1)
  expect_equal(coefs.polynomial(p, 2), 4)
  expect_equal(coefs.polynomial(p, 20), 0)

  expect_equal(coefs(p), c(1, 3, 4, 5, 6))
})


test_that("coefs.polyMatrix", {
  pm <- polyMgen.a(degree=c(1, 2, 3, 2, 1, 0))
  C0 <- matrix(c(1,  6, 13,
                 3, 10, 15), 2, 3, byrow=TRUE)
  C1 <- matrix(c(2,  7, 14,
                 4, 11, 0), 2, 3, byrow=TRUE)
  C2 <- matrix(c(0,  8, 0,
                 5, 12, 0), 2, 3, byrow=TRUE)
  C3 <- matrix(c(0,  9, 0,
                 0, 0, 0), 2, 3, byrow=TRUE)
  expect_true(all(coefs.polyMatrix(pm)[[1]] == C0))
  expect_true(all(coefs.polyMatrix(pm)[[2]] == C1))
  expect_true(all(coefs.polyMatrix(pm)[[3]] == C2))
  expect_true(all(coefs.polyMatrix(pm)[[4]] == C3))
  expect_true(all(coefs.polyMatrix(pm, 0)[[1]] == C0))
  expect_true(all(coefs.polyMatrix(pm, 2)[[1]] == C2))
  expect_true(all(coefs.polyMatrix(pm, 5)[[1]] == 0))

  expect_true(all(coefs(pm)[[1]] == C0))
})
