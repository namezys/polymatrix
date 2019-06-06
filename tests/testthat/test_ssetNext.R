context("ssetNext")

test_that("ssetNext", {
  a <- c(0, 1, 1, 1)
  expect_equal(ssetNext(a), c(1, 0, 0, 0))
  
  b <- c(5, 3, 2, 4)
  expect_equal(ssetNext(b), 4)
  
  c <- c(5, 0, 5, 0)
  expect_equal(ssetNext(c), c(5, 0, 5, 1))
})
