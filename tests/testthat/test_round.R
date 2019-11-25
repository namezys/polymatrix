context("test_round")

p <- polynom::polynomial

test_that("round", {
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


test_that("round.polynomial", {
  expect_equal(round(p(c(0, 0.1, 1.5, 1, 1.5))), p(c(0, 0, 2, 1, 2)))
  expect_equal(round(p(c(0, 0.11, .59, 1.3, 1.51))), p(c(0, 0, 1, 1, 2)))
  expect_equal(round(p(c(0, 0.11, .59, 1.3, 1.51)), 1), p(c(0, 0.1, .6, 1.3, 1.5)))
})


test_that("is.zero.polyMatrxi", {
  src <- polyMgen.d(nrow=3, ncol=2 ,byrow=TRUE, ch2pn(c(
    "1.59*s - 1.59", "s^2 - 1.2",
    "2            ", "2.49*s + 2",
    "0            ", "3"
    ), symb="s"), symb="s")

  res_0 <- polyMgen.d(nrow=3, ncol=2 ,byrow=TRUE, ch2pn(c(
    "2*s - 2", "s^2 - 1",
    "2      ", "2*s + 2",
    "0      ", "3"
    ), symb="s"), symb="s")

  res_1 <- polyMgen.d(nrow=3, ncol=2 ,byrow=TRUE, ch2pn(c(
    "1.6*s - 1.6", "s^2 - 1.2",
    "2          ", "2.5*s + 2",
    "0          ", "3"
    ), symb="s"), symb="s")
  expect_equal(round(src), res_0)
  expect_equal(round(src, 1), res_1)
})

test_that("is.zero.polyMatrxi_degree", {
  src <- polyMgen.d(nrow=3, ncol=2 ,byrow=TRUE, ch2pn(c(
    "0.09*s - 1.59", "0.09 * s^2      - 0.2",
    "2            ", "2.49*s + 2",
    "0            ", "3"
  ), symb="s"), symb="s")

  res_0 <- polyMgen.d(nrow=3, ncol=2 ,byrow=TRUE, ch2pn(c(
    "-2", "0",
    "2 ", "2*s + 2",
    "0 ", "3"
  ), symb="s"), symb="s")

  expect_equal(round(src), res_0)
})
