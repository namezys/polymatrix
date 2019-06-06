context("permNext")

test_that("permNext", {
  a <- c(1, 2, 3, 4)
  expect_equal(permNext(a), c(1, 2, 4, 3))
  
  b <- c(4)
  expect_equal(permNext(b), c(1, 2, 3, 4))
})
