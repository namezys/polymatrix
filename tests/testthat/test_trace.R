context("trace")

test_that("tr.matrix", {
  a = matrix(1:18, 3, 6)
  b = matrix(3:11, 3, 3)
  c = matrix(30:55, 13, 2)
  
  expect_equal(tr.matrix(a), 15)
  expect_equal(tr.matrix(b), 21)
  expect_equal(tr.matrix(c), 74)
})

test_that("tr.polyMatrix", {
  a = polyMgen(2, 3, 1:16, "x", FALSE, 2, FALSE)
  b = polyMgen(3, 5, 10:52, "x", FALSE, 2, FALSE)
  c = polyMgen(6, 4, 1:70, "x", FALSE, 2, FALSE)
  
  expect_equal(tr.polyMatrix(a), polynomial(c(11, 13, 15)))
  expect_equal(tr.polyMatrix(b), polynomial(c(66, 69, 72)))
  expect_equal(tr.polyMatrix(c), polynomial(c(130, 134, 138)))
})