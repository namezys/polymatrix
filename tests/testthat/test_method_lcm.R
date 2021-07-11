context("method LCM")

test_that("LCM", {
  init_data()

  expect_equal(LCM(pm_first), p(0))
  expect_equal(LCM(parse.polyMatrix("1 + 3x,      2, 2 + 3x",
                                    "     3, 1 + 3x, 1 + 3x")),
               p(12, 54, 54))
  expect_equal(LCM(parse.polyMatrix("  1 + 3x, 1 + 6x + 9x^2",
                                    "1 - 9x^2,        1 + 3x")),
               p(0.5, 1.5, -4.5, -13.5))
})
