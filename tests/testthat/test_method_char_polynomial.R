context("method charpolynom")

test_that("numerical", {
  expect_equal(charpolynom(matrix(2, 1, 1)), p(2, -1))
  expect_equal(charpolynom(matrix(c(2, 1, 3, 0), 2, 2, byrow = TRUE)), p(-3, -2, 1))
})

test_that("polyMatrix", {
  init_data()
  expect_equal(charpolynom(pm_first[1, 1])@coef, parse.polyMatrix("1 + x + 2 x^2, -1"))
  expect_equal(charpolynom(pm_first[1:2, 1:2])@coef,
               parse.polyMatrix("2 - x + 3x^2 - 12x^3 -4x^4, -3 -x -2x^2, 1"))
  expect_equal(charpolynom(pm_first)@coef,
               parse.polyMatrix(
                 "2 - 19x + 3x^2 - 12x^3 + 50x^4 + 18x^5, -5 + 9x - 5x^2 + 12x^3 + 4x^4, 4 + x + 2x^2, -1"
               ))
})
