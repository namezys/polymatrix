context("permSign")

test_that("permSign", {
  a <- c(1, 2, 3, 4)
  expect_equal(permSign(a), 1)
  
  b <- c(1, 2, 4, 3)
  expect_equal(permSign(b), -1)
  
  c <- c(3, 4, 1, 2)
  expect_equal(permSign(c), 1)
})

