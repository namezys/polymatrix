context("utils rowmin colmin")

test_that("new_colMin with matrix", {
  mtr1 = matrix(c(5:34), 5, 6)
  a = new_colMin(mtr1)
  expect_equal(a, seq(5, 30, 5))
  
  mtr2 = matrix(seq(4, 23, 2), 2, 5)
  b = new_colMin(mtr2)
  expect_equal(b, seq(4, 20, 4))
})

test_that("new_rowMin with matrix", {
  mtr1 = matrix(c(5:34), 5, 6)
  a = new_rowMin(mtr1)
  expect_equal(a, c(5:9))
  
  mtr2 = matrix(seq(4, 23, 2), 2, 5)
  b = new_rowMin(mtr2)
  expect_equal(b, c(4, 6))
})
