test_that("new_cycFill with numerical data", {
  a = new_cycFill(c(1,2,3,4), 6)
  expect_equal(a, c(1,2,3,4,1,2))
  
  b = new_cycFill(c(5,6,7), 8)
  expect_equal(b, c(5,6,7,5,6,7,5,6))
})