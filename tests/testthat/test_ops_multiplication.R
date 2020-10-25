context("ops miltiplication")

test_that("by number", {
  op <- parse.polyMatrix("1, 2 + x, 3 + x^2",
                         "x,   x^3, -6 + x^2")

  expect_equal(op * 3, parse.polyMatrix("  3, 6 + 3x, 9 + 3x^2",
                                        " 3x,   3x^3, -18 + 3x^2"))
  expect_equal((-2) * op, parse.polyMatrix("  -2, -4 - 2x, -6 - 2x^2",
                                           "-2 x,   -2 x^3, 12 - 2x^2"))
})

test_that("by polynomial", {
  op <- parse.polyMatrix("1, 2 + x, 3 + x^2",
                         "x,   x^3, -6 + x^2")
  expect_equal(op * parse.polynomial("1 - x"),
               parse.polyMatrix("  1 - x, 2 - x - x^2, 3 -3x + x^2 - x^3",
                                "x - x^2,   x^3 - x^4, -6 + 6x +x^2 - x^3"))
  expect_equal(parse.polynomial("10x^2") * op,
               parse.polyMatrix("10x^2, 20 x^2 + 10 x^3, 30x^2 + 10x^4",
                                "10x^3,          10 x^5, -60 x^2 + 10 x^4")
  )
})

test_that("by polynomial", {
  init_data()

  expect_error(pm_first * pm_first, "support")
})