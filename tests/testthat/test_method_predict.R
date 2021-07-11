context("method predict")

test_that("polyMatrix by number", {
  init_data()

  expect_equal(predict(pm_first, 0), matrix(c(1, 0, 3, 1, 2, 0, 0, 0, 1), 3, 3))
  expect_equal(predict(pm_first, 1), matrix(c(4, 4, 3, 5, 2, 6, 3, 0, 1), 3, 3))
  expect_equal(predict(pm_first, -1), matrix(c(2, -2, 3, 5, 2, 6, -3, 0, 1), 3, 3))
})

test_that("polyMatrix by polynomial", {
  init_data()

  expect_equal(predict(pm_first, p(1, 1)),
               parse.polyMatrix(
                 "   4 + 5x + 2x^2,    5 + 8x + 4x^2,   3 + 3x",
                 "    4 + 5x + x^2,                2,        0",
                 "               3,   6 + 12x + 6x^2,        1"
               ))
  predict(pm_first, p(1, 1))
})

test_that("char poly of polyMatrix", {
  expect_equal(predict(pmcp, 0), p(1, 1))
  expect_equal(predict(pmcp, 1), p(0, 2, 1))
  expect_equal(predict(pmcp, p(0, 1)), p(1, 1, 0, -1, 2))
})