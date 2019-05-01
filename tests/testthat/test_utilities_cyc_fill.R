context("utils cyc fill")

test_that("new_cycFill with numerical data", {
  a = cycFill(c(1,2,3,4), 6)
  expect_equal(a, c(1,2,3,4,1,2))
  
  b = cycFill(c(5,6,7), 8)
  expect_equal(b, c(5,6,7,5,6,7,5,6))
})

test_that("new_cycFill with polynomail data", {
  c = cycFill((polynomial(c(3, 6))), 8)
  expect_equal(c, c(3,6,3,6,3,6,3,6))
  
  d = cycFill((polynomial(c(5,11))), 13)
  expect_equal(d, c(5,11,5,11,5,11,5,11,5,11,5,11,5))
})

test_that("new_cycFill with list of polynomails data", {
  e = cycFill(list(polynomial(c(5,9)), polynomial(c(2,11)), polynomial(c(9,14))), 6)
  expect_equal(e[[1]], polynomial(c(5,9)))
  expect_equal(e[[2]], polynomial(c(2,11)))
  expect_equal(e[[3]], polynomial(c(9,14)))
  expect_equal(e[[4]], polynomial(c(5,9)))
  expect_equal(e[[5]], polynomial(c(2,11)))
  expect_equal(e[[6]], polynomial(c(9,14)))
})
