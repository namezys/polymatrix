context("ops equal")

test_that("with number", {
  expect_equal(
    parse.polyMatrix("      1, 1 + x, 1 + x^2",
                     "1 + x^2, 1 + x, 1")
      ==
      1,
    matrix(c(TRUE, FALSE, FALSE, FALSE, FALSE, TRUE), 2, 3, byrow = TRUE)
  )
})

test_that("with polynomail", {

  expect_equal(
    parse.polyMatrix("      1, 1 + x, 1 + x^2",
                     "1 + x^2, 1 + x, 1")
      ==
      parse.polynomial("1 + x"),
    matrix(c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE), 2, 3, byrow = TRUE)
  )
  expect_equal(
    parse.polynomial("1 + x^2") ==
      parse.polyMatrix("      1, 1 + x, 1 + x^2",
                       "1 + x^2, 1 + x, 1"),
    matrix(c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE), 2, 3, byrow = TRUE)
  )
  expect_equal(
    parse.polynomial("1 + x^3") ==
      parse.polyMatrix("      1, 1 + x, 1 + x^2",
                       "1 + x^2, 1 + x, 1"),
    matrix(FALSE, 2, 3)
  )
  expect_equal(
    parse.polynomial("1 + x^3") !=
      parse.polyMatrix("      1, 1 + x, 1 + x^2",
                       "1 + x^2, 1 + x, 1"),
    matrix(TRUE, 2, 3)
  )
})

test_that("with matrix", {
  expect_equal(
    parse.polyMatrix("      1, 1 + x, 1 + x^2",
                     "1 + x^2, 1 + x, 1")
      ==
      matrix(c(1, 2, 3, 4, 5, 6), 2, 3, byrow = TRUE),
    matrix(c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE), 2, 3, byrow = TRUE)
  )
  expect_equal(
    parse.polyMatrix("      1, 1 + x, 1 + x^2",
                     "1 + x^2, 1 + x, 1")
      !=
      matrix(1, 2, 3, byrow = TRUE),
    matrix(c(FALSE, TRUE, TRUE, TRUE, TRUE, FALSE), 2, 3, byrow = TRUE)
  )
  expect_error(polyMatrix(0, 1, 2) == matrix(0, 2, 2), "non-conformable")
  expect_error(matrix(0, 2, 2) == polyMatrix(0, 2, 3), "non-conformable")
})

test_that("with polyMatrix", {
  expect_equal(
    parse.polyMatrix("      1, 1 + x, 1 + x^2",
                     "1 + x^2, 1 + x, 1")
      ==
      parse.polyMatrix("    1, 1 + x, 1 + x^2",
                       "    1,     1, x^2"),
    matrix(c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE), 2, 3, byrow = TRUE)
  )

  expect_equal(
    parse.polyMatrix("      1, 1 + x, 1 + x^2",
                     "1 + x^2, 1 + x, 1")
      ==
      parse.polyMatrix("    1, 1 + x, 1",
                       "    1,     1, 1"),
    matrix(c(TRUE, TRUE, FALSE, FALSE, FALSE, TRUE), 2, 3, byrow = TRUE)
  )

  expect_equal(
    parse.polyMatrix("1, 1 + x, 1",
                     "1,     1, 1 + x")
      ==
      parse.polyMatrix("    1, 1 + x, 1 + x^2",
                       "    1,     1, x^2"),
    matrix(c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE), 2, 3, byrow = TRUE)
  )

  expect_equal(
    parse.polyMatrix("      1, 1 + x, 1 + x^2",
                     "1 + x^2, 1 + x, 1")
      !=
      parse.polyMatrix("    1, 1 + x, 1 + x^2",
                       "    1,     1, x^2"),
    matrix(c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE), 2, 3, byrow = TRUE)
  )

  expect_error(polyMatrix(0, 1, 2) == polyMatrix(0, 2, 2), "non-conformable")
  expect_error(polyMatrix(0, 1, 2) == polyMatrix(0, 2, 2), "non-conformable")
  expect_error(polyMatrix(0, 1, 2) != polyMatrix(0, 2, 2), "non-conformable")
  expect_error(polyMatrix(0, 1, 2) != polyMatrix(0, 2, 2), "non-conformable")
})
