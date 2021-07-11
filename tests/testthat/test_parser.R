context("parser")


test_that("polynomial", {
  expect_equal(parse.polynomial("0"), p(0))
  expect_equal(parse.polynomial("1"), p(1))
  expect_equal(parse.polynomial("+1"), p(1))
  expect_equal(parse.polynomial("-1"), p(-1))
  expect_equal(parse.polynomial(".7"), p(0.7))
  expect_equal(parse.polynomial("-.7"), p(-0.7))
  expect_equal(parse.polynomial("1e10"), p(1e10))
  expect_equal(parse.polynomial("-.4e-7"), p(-0.4e-7))
  expect_equal(parse.polynomial("x^0"), p(1))

  expect_equal(parse.polynomial("x "), p(0, 1))
  expect_equal(parse.polynomial("-x"), p(0, -1))
  expect_equal(parse.polynomial(" 1x"), p(0, 1))
  expect_equal(parse.polynomial("-2 x"), p(0, -2))
  expect_equal(parse.polynomial("-2.0e+7x"), p(0, -2e7))
  expect_equal(parse.polynomial("+.2e-6x"), p(0, 2e-7))

  expect_equal(parse.polynomial("x"), p(0, 1))
  expect_equal(parse.polynomial("1* x"), p(0, 1))
  expect_equal(parse.polynomial("-2 *x"), p(0, -2))
  expect_equal(parse.polynomial("-2.0e+7* x"), p(0, -2e7))
  expect_equal(parse.polynomial("+.2e-6 * x"), p(0, 2e-7))

  expect_equal(parse.polynomial("x ^  2"), p(0, 0, 1))
  expect_equal(parse.polynomial(" 1x ^ 1"), p(0, 1))
  expect_equal(parse.polynomial("-2 x"), p(0, -2))
  expect_equal(parse.polynomial("-2.0e+7x"), p(0, -2e7))
  expect_equal(parse.polynomial("+.2e-6x"), p(0, 2e-7))

  expect_equal(parse.polynomial("1-x"), p(1, -1))
  expect_equal(parse.polynomial("3 * x + 12 - 0.1x^3 + x^2"), p(12, 3, 1, -0.1))
  expect_equal(parse.polynomial("3 * a + 12 - 0.1a^3 + a^2", "a"), p(12, 3, 1, -0.1))

  expect_error(parse.polynomial(""), "empty")
  expect_error(parse.polynomial("+"), "1")
  expect_error(parse.polynomial("!"), "1")
  expect_error(parse.polynomial("s", var = "x"), "1")
  expect_error(parse.polynomial("^"), "1")
  expect_error(parse.polynomial("122^334"), "1")

  expect_error(parse.polynomial("0", "xx"), "invalid .*name")
  expect_error(parse.polynomial("0", "e"), "invalid .*name")
  expect_error(parse.polynomial("0", "1"), "invalid .*name")
})

test_that("polyMatrix", {
  expect_equal(parse.polyMatrix("1; 2; x& x^4
                                    3, 2; 4;   3; 4"),
               polyMatrix(matrix(c(1, 2, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
                                   3, 2, 4, 3, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                                 2, 25, byrow = TRUE),
                          2, 5, 4)
  )
})