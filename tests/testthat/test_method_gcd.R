context("method GCD")

test_that("GCD", {
  init_data()

  expect_equal(GCD(pm_first), p(1))
  expect_equal(GCD(parse.polyMatrix("1 + 3x,      2, 2 + 3x",
                                    "     3, 1 + 3x, 1 + 4x")),
               p(1))
  expect_equal(GCD(parse.polyMatrix("  1 + 3x, 1 + 6x + 9x^2",
                                    "1 - 9x^2,        1 + 3x")),
               p(1, 3))
})
