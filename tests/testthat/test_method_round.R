context("method round")


test_that("numerical", {
  expect_equal(round(1), 1)
  expect_equal(round(1.1), 1)
  expect_equal(round(1.5), 2)

  expect_equal(round(c(1, 1.1, 1.5, 1.9)), c(1, 1, 2, 2))
  expect_equal(round(matrix(c(1, 1.1, 1.5, 1.9), 2, 2)), matrix(c(1, 1, 2, 2), 2, 2))

  expect_equal(round(1, 1), 1)
  expect_equal(round(1.1, 1), 1.1)
  expect_equal(round(1.19, 1), 1.2)
  expect_equal(round(1.5, 1), 1.5)

  expect_equal(round(c(1, 1.1, 1.5, 1.99), 1), c(1, 1.1, 1.5, 2))
  expect_equal(round(matrix(c(1, 1.1, 1.5, 1.99), 2, 2), 1), matrix(c(1, 1.1, 1.5, 2), 2, 2))
})

test_that("polynomial", {
  expect_equal(round(p(0, 0.1, 1.5, 1, 1.5)), p(0, 0, 2, 1, 2))
  expect_equal(round(p(0, 0.11, .59, 1.3, 1.51)), p(0, 0, 1, 1, 2))
  expect_equal(round(p(0, 0.11, .59, 1.3, 1.51), 1), p(0, 0.1, .6, 1.3, 1.5))
})


test_that("polyMatrix", {
  src <- parse.polyMatrix("1.59x - 1.59; x^2 - 1.2",
                          "x           ; 2.49x + 2",
                          "0           ; 3")

  res_first <- parse.polyMatrix("2x - 2; x^2 - 1",
                                "x     ; 2x  + 2",
                                "0     ; 3")
  res_second <- parse.polyMatrix("1.6x - 1.6; x^2 - 1.2",
                                 "x         ; 2.5x + 2",
                                 "0         ; 3")
  expect_equal(round(src), res_first)
  expect_equal(round(src, 1), res_second)
})

test_that("polyMatrxi second", {
  src <- parse.polyMatrix("0.09x - 1.59;      0.09x^2",
                          "2           ; 2.49x + 2",
                          "0           ; 3")

  res <- parse.polyMatrix("-2; 0",
                          " 2; 2*x + 2",
                          " 0; 3")

  expect_equal(round(src), res)
})
