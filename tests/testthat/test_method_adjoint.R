context("method adjoint")

test_that("numerical", {
  m <- matrix(c(1, 2, 3,
                3, 2, 1,
                1, 1, 1), 3, 3, byrow = TRUE)
  expect_equal(cofactor(m, 1, 1), 1)
  expect_equal(cofactor(m, 1, 2), -2)
  expect_equal(cofactor(m, 2, 1), 1)

  expect_equal(adjoint(m), matrix(c(1, 1, -4,
                                    -2, -2, 8,
                                    1, 1, -4), 3, 3, byrow = TRUE))
})

test_that("cofactor polyMatrix", {
  init_data()

  expect_equal(cofactor(pm_first, 1, 1), p(2))
  expect_equal(cofactor(pm_first, 2, 2), p(1, -8, 2))
  expect_equal(cofactor(pm_first, 1, 2), p(0, -3, -1))
  expect_equal(cofactor(pm_first, 2, 1), p(-1, 0, -4, 18))

  expect_equal(adjoint(pm_first), parse.polyMatrix(
    "                  2,         -1 -4 x^2 + 18 x^3,                            -6x",
    "          -3x - x^2,              1 - 8x + 2x^2,                   9x^2 + 3 x^3",
    "-6 + 18*x^3 + 6*x^4, 3 + 6*x^2 - 6*x^3 - 12*x^4, 2 - x + 3*x^2 - 12*x^3 - 4*x^4"
  ))
})