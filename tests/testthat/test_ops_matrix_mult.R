context("ops matrix mulitpolication")


test_that("polyMaytix by polyMaytix", {
  first <- parse.polyMatrix("      1, 1 + x",
                            "2 + x^2, x - x^2",
                            "     -x, 3")
  second <- parse.polyMatrix("1 + x, x^2, 2 - x",
                             "    x, -3x, x")

  expect_equal(first %*% second,
               parse.polyMatrix("1 + 2x +  x^2,           - 3x - 2x^2,                2 + x^2",
                                "2 + 2x + 2x^2,    - x^2 + 3x^3 + x^4,   4 - 2x + 3x^2 - 2x^3",
                                "    2x -  x^2,            - 9x - x^3,                x + x^2"))
})