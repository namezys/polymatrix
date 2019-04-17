context("utils cyc fill")

test_that("new_cycFill with numerical data", {
  a = new_cycFill(c(1,2,3,4), 6)
  expect_equal(a, c(1,2,3,4,1,2))
  
  b = new_cycFill(c(5,6,7), 8)
  expect_equal(b, c(5,6,7,5,6,7,5,6))
})

test_that("new_cycFill with polynomail data", {
  c = new_cycFill((polynomial(c(3, 6))), 8)
  expect_equal(c, c(3,6,3,6,3,6,3,6))
  
  d = new_cycFill((polynomial(c(5,11))), 13)
  expect_equal(d, c(5,11,5,11,5,11,5,11,5,11,5,11,5))
})
