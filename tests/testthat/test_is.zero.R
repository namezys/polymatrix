context("test_is_zero")

p <- polynom::polynomial

test_that("is.zero", {
  expect_true(is.zero(0))
  expect_true(is.zero(.Machine$double.eps))
  expect_equal(is.zero(c(0, 0, 0)), c(TRUE, TRUE, TRUE))
  expect_equal(is.zero(c(0, .Machine$double.eps)), c(TRUE, TRUE))
  expect_equal(
    is.zero(matrix(c(0, .Machine$double.eps, 0, 0), 2, 2)),
    matrix(c(TRUE, TRUE, TRUE, TRUE), 2, 2)
  )

  expect_true(is.zero(-1, eps=1.1))
  expect_equal(is.zero(c(0, 1, 0), eps=1.1), c(TRUE, TRUE, TRUE))
  expect_equal(
    is.zero(matrix(c(0, 1, 0, 1), 2, 2), eps=1.1),
    matrix(c(TRUE, TRUE, TRUE, TRUE), 2, 2)
  )

  expect_false(is.zero(1))
  expect_equal(is.zero(c(0, 1, 0)), c(TRUE, FALSE, TRUE))
  expect_equal(
    is.zero(matrix(c(0, 1, 0, 0), 2, 2)),
    matrix(c(TRUE, FALSE, TRUE, TRUE), 2, 2)
  )

  expect_false(is.zero(1, eps=0.1))
  expect_equal(is.zero(c(0, 1, 0), eps=0.1), c(TRUE, FALSE, TRUE))
  expect_false(all(is.zero(matrix(c(0, 1, 0, 0), 2, 2), eps=0.1)))
})


test_that("is.zero.polynomial", {
  expect_true(is.zero(p(c(0))))
  expect_true(is.zero(p(c(0, 0))))
  expect_true(is.zero(p(c(0, 0, 0))))
  expect_true(is.zero(p(c(1, 1)) - p(c(1, 1))))

  expect_true(is.zero(p(c(.Machine$double.eps))))
  expect_true(is.zero(p(c(0, .Machine$double.eps))))
  expect_true(is.zero(p(c(.Machine$double.eps, 0, .Machine$double.eps))))
  expect_true(is.zero(p(c(.Machine$double.eps, 1)) - p(c(0, 1 + .Machine$double.eps))))

  expect_true(is.zero(p(c(1)), eps=1.1))
  expect_true(is.zero(p(c(1, -1)), eps=1.1))
  expect_true(is.zero(p(c(0, -1, 1)), eps=1.1))
  expect_true(is.zero(p(c(-2, 2)) - p(c(-1, 1)), eps=1.1))

  expect_false(is.zero(p(c(1))))
  expect_false(is.zero(p(c(1, -1))))
  expect_false(is.zero(p(c(0, -1, 1))))
  expect_false(is.zero(p(c(-2, 2)) - p(c(-1, 1))))

  expect_false(is.zero(p(c(1)), eps=0.1))
  expect_false(is.zero(p(c(1, -1)), eps=0.1))
  expect_false(is.zero(p(c(0, -1, 1)), eps=0.1))
  expect_false(is.zero(p(c(-2, 2)) - p(c(-1, 1)), eps=0.1))

})


test_that("is.zero.polyMatrxi", {
  M_0 <- polyMgen.d(nrow=3, ncol=2 ,byrow=TRUE, ch2pn(c(
    "s - 1", "s^2 - 1",
    "2    ", "2*s + 2",
    "0    ", "3"
    ), symb="s"), symb="s")

  expect_equal(is.zero(M_0 - M_0), matrix(TRUE, 3, 2))
  expect_equal(is.zero(M_0, eps=3.1), matrix(TRUE, 3, 2))

  expect_equal(is.zero(M_0), matrix(c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE), 3, 2))
  expect_equal(is.zero(M_0, eps=1.9), matrix(c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE), 3, 2))
})
