context("method t")

test_that("t.polyMatrix", {
  a <- parse.polyMatrix("-3 + x^2, 2 + 4*x, -x^2",
                        "       1,       2, 3 + x",
                        "     2*x,       0, 2 - 3*x")
  expect_equal(t(a),
               parse.polyMatrix("-3 + x^2,     1, 2*x",
                                " 2 + 4*x,     2, 0",
                                "    -x^2, 3 + x, 2 - 3*x"))


  b <- parse.polyMatrix("3 + x^2, 2 + 2*x",
                        "   -x^2, 1")

  expect_equal(t(b), parse.polyMatrix("3 + x^2, -x^2",
                                      "2 + 2*x, 1"))
})
