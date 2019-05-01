context("utils rowmin colmin")

test_that("colMin with matrix", {
  mtr1 = matrix(c(5:34), 5, 6)
  a = colMin(mtr1)
  expect_equal(a, seq(5, 30, 5))
  
  mtr2 = matrix(seq(4, 23, 2), 2, 5)
  b = colMin(mtr2)
  expect_equal(b, seq(4, 20, 4))
})

test_that("rowMin with matrix", {
  mtr1 = matrix(c(5:34), 5, 6)
  a = rowMin(mtr1)
  expect_equal(a, c(5:9))
  
  mtr2 = matrix(seq(4, 23, 2), 2, 5)
  b = rowMin(mtr2)
  expect_equal(b, c(4, 6))
})
