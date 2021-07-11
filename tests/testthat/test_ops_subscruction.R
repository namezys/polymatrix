context("ops subscrutction")

test_that("unary", {
  pm <- parse.polyMatrix("1,   1 + x, -3 + 2x^2",
                         "0, x - x^2, 3 - x")
  expect_equal(-pm, parse.polyMatrix("-1,   -1 - x, 3 -2x^2",
                                     " 0, -x + x^2, -3 + x"))
})

test_that("with number", {
  pm <- parse.polyMatrix("  0,       1, 1 + x, x",
                         "x^2, 1 + x^2,     2, 0")
  expect_equal(pm - 11, parse.polyMatrix("      -11,       -10, -10 + x, -11 + x",
                                         "-11 + x^2, -10 + x^2,      -9, -11"))
  expect_equal(-2 - pm, parse.polyMatrix("      -2,       -3, -3 - x, -2 - x",
                                         "-2 - x^2, -3 - x^2,     -4, -2"))
})

test_that("with polynomial", {
  pm <- parse.polyMatrix("  0, x",
                         "x^2, 3",
                         "1 + x, 1 + x^2")
  expect_equal(pm - parse.polynomial("1 + x"),
               parse.polyMatrix("      -1 - x, -1",
                                "-1 - x + x^2, 2 - x",
                                "           0, -x + x^2"))
  expect_equal(parse.polynomial("1 - x^2") - pm,
               parse.polyMatrix("    1 - x^2, 1 - x - x^2",
                                "   1 - 2x^2, -2 - x^2",
                                "   -x - x^2, - 2x^2"))
})

test_that("with matrix", {
  pm <- parse.polyMatrix("  0, x",
                         "x^2, 3",
                         "1 + x, 1 + x^2")
  expect_equal(pm - matrix(1:6, 3, 2, byrow = TRUE),
               parse.polyMatrix("      -1, -2 + x",
                                "-3 + x^2, -1",
                                "-4 + x, -5 + x^2"))
  expect_equal(matrix(1:6, 3, 2) - pm,
               parse.polyMatrix("      1, 4 - x",
                                "2 - x^2, 2",
                                "  2 - x, 5 - x^2"))

  expect_error(pm + matrix(0, 1, 2), "non-conformable")
})

test_that("with polyMatrix", {
  first <- parse.polyMatrix("1 + x, 1 + x^2",
                            "    2, 2 + 3x")
  second <- parse.polyMatrix("1, 2 + x",
                             "-3, -4 - 3x")
  expect_equal(first - second, parse.polyMatrix("x, -1 -x + x^2",
                                                "5, 6 + 6x"))
  expect_equal(second - first, parse.polyMatrix("-x, 1 + x - x^2",
                                                "-5, -6 - 6x"))

  expect_error(first + polyMatrix(0, 1, 2), "non-conformable")
})