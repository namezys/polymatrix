context("utils rowmax colmax")

test_that("colMin with matrix", {
  mtr1 = matrix(c(5:34), 5, 6)
  a = colMax(mtr1)
  expect_equal(a, seq(9, 34, 5))
  
  mtr2 = matrix(seq(4, 23, 2), 2, 5)
  b = colMax(mtr2)
  expect_equal(b, seq(6, 22, 4))
})

test_that("rowMin with matrix", {
  mtr1 = matrix(c(5:34), 5, 6)
  a = rowMax(mtr1)
  expect_equal(a, c(30:34))
  
  mtr2 = matrix(seq(4, 23, 2), 2, 5)
  b = rowMax(mtr2)
  expect_equal(b, c(20, 22))
})
