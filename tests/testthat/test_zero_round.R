context("method zero.round")


test_that("numbers", {
  expect_equal(zero.round(1), 1)
  expect_equal(zero.round(0), 0)
  expect_equal(zero.round(1.1), 1.1)
  expect_equal(zero.round(1e-100), 0)
  expect_equal(zero.round(-1e-100), 0)

  expect_equal(zero.round(c(1, 0, 2, 1.1, 1e-100, 2)), c(1, 0, 2, 1.1, 0, 2))
  expect_equal(zero.round(matrix(c(1, 0, 2, 1.1, -1e-100, 2), 2, 3)),
               matrix(c(1, 0, 2, 1.1, 0, 2), 2, 3))

  expect_equal(zero.round(1, 1.05), 0)
  expect_equal(zero.round(-1, 1.05), 0)
  expect_equal(zero.round(0, 1.05), 0)
  expect_equal(zero.round(1.1, 1.05), 1.1)
  expect_equal(zero.round(1e-100, 1.05), 0)
  expect_equal(zero.round(-1e-100, 1.05), 0)

  expect_equal(zero.round(c(-1, 0, 2, 1.1, 1e-100, 2), 1.05), c(0, 0, 2, 1.1, 0, 2))
  expect_equal(zero.round(matrix(c(1, 0, 2, 1.1, 1e-100, 2), 2, 3), 1.05),
               matrix(c(0, 0, 2, 1.1, 0, 2), 2, 3))
})


test_that("polynomial", {
  s <- p(1.5, 1, -1e-100)
  res <- zero.round(s)
  expect_equal(res, p(c(1.5, 1)))
  expect_equal(as.numeric(res), c(1.5, 1))

  th_res <- zero.round(s, 1.05)
  expect_equal(th_res, p(1.5))
  expect_equal(as.numeric(th_res), 1.5)
})


test_that("polyMatrix", {
  src <- parse.polyMatrix("            x - 1.5, 2 * x^2 - 1",
                          "0.00000000000000001, 2.49x - 0.00000000000000001",
                          "                  0, 0.00000000000000001 * x + 5")

  res <- parse.polyMatrix(" 1x - 1.5, 2 * x^2 - 1",
                          "0        , 2.49*x",
                          "0        , 5")

  th_res <- parse.polyMatrix("-1.5, 2 * x^2",
                             "0   , 2.49*x",
                             "0   , 5")

  expect_equal(zero.round(src), res)
  expect_equal(zero.round(src, 1.05), th_res)
})
