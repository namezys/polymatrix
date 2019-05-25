context("trace")

test_that("tr.matrix", {
  a = matrix(1:18, 3, 6)
  b = matrix(3:11, 3, 3)
  
  expect_equal(tr.matrix(a), 15)
  expect_equal(tr.matrix(b), 21)
})

test_that("tr.polyMatrix", {
  a = polyMgen(2, 3, 1:6, "x", FALSE, 2, FALSE)
  
  expect_equal(tr.polyMatrix(a), polynomial(c(5, 7, 9)))
})