context("test_zero_round")

p <- polynom::polynomial

test_that("zero_round", {
  expect_equal(zero_round(1), 1)
  expect_equal(zero_round(0), 0)
  expect_equal(zero_round(1.1), 1.1)
  expect_equal(zero_round(1e-100), 0)
  expect_equal(zero_round(-1e-100), 0)

  expect_equal(zero_round(c(1, 0, 2, 1.1, 1e-100, 2)), c(1, 0, 2, 1.1, 0, 2))
  expect_equal(zero_round(matrix(c(1, 0, 2, 1.1, -1e-100, 2), 2, 3)), matrix(c(1, 0, 2, 1.1, 0, 2), 2, 3))

  expect_equal(zero_round(1, 1.05), 0)
  expect_equal(zero_round(-1, 1.05), 0)
  expect_equal(zero_round(0, 1.05), 0)
  expect_equal(zero_round(1.1, 1.05), 1.1)
  expect_equal(zero_round(1e-100, 1.05), 0)
  expect_equal(zero_round(-1e-100, 1.05), 0)

  expect_equal(zero_round(c(-1, 0, 2, 1.1, 1e-100, 2), 1.05), c(0, 0, 2, 1.1, 0, 2))
  expect_equal(zero_round(matrix(c(1, 0, 2, 1.1, 1e-100, 2), 2, 3), 1.05), matrix(c(0, 0, 2, 1.1, 0, 2), 2, 3))
})


test_that("zero_round.polynomial", {
  s = p(c(1.5, 1, -1e-100))
  res = zero_round(s)
  expect_equal(res, p(c(1.5, 1)))
  expect_equal(as.numeric(res), c(1.5, 1))

  th_res = zero_round(s, 1.05)
  expect_equal(th_res, p(c(1.5)))
  expect_equal(as.numeric(th_res), c(1.5))
})


test_that("zero_round.polyMatrix", {
  src <- polyMgen.d(nrow=3, ncol=2 ,byrow=TRUE, ch2pn(c(
    "1*s - 1.5          ", "2 * s^2 - 1",
    "0.00000000000000001", "2.49*s - 0.00000000000000001",
    "0                  ", "0.00000000000000001 * s + 5"
    ), symb="s"), symb="s")

  res <- polyMgen.d(nrow=3, ncol=2 ,byrow=TRUE, ch2pn(c(
    "1*s - 1.5", "2 * s^2 - 1",
    "0        ", "2.49*s",
    "0        ", "5"
    ), symb="s"), symb="s")

  th_res <- polyMgen.d(nrow=3, ncol=2 ,byrow=TRUE, ch2pn(c(
    "-1.5", "2 * s^2",
    "0   ", "2.49*s",
    "0   ", "5"
  ), symb="s"), symb="s")

  expect_equal(zero_round(src), res)
  expect_equal(zero_round(src, 1.05), th_res)
})
