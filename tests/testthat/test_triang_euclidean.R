context("coefs")

p <- polynom::polynomial


test_that("is_polynomail_monic", {
  expect_true(is_polynomail_monic(polynom::polynomial(c(1, 3, 4, 5, 1))))
  expect_true(is_polynomail_monic(polynom::polynomial(c(0, 1))))
  expect_true(is_polynomail_monic(polynom::polynomial(c(1))))

  expect_false(is_polynomail_monic(polynom::polynomial(c(1, 3, 4, 5, 2))))
  expect_false(is_polynomail_monic(polynom::polynomial(c(0, 2))))
  expect_false(is_polynomail_monic(polynom::polynomial(c(3))))
})

test_that("is_polynomial_zero", {
  expect_false(is_polynomial_zero(polynom::polynomial(c(1, 3, 4, 5, 6))))
  p <- polynom::polynomial(c(0, 1))
  expect_false(is_polynomial_zero(p))

  expect_true(is_polynomial_zero(polynom::polynomial(c(0))))
  p[2] <- 0
  expect_true(is_polynomial_zero(p))
})

test_that("is_any_polynomail_nonzero", {
  p <- polynom::polynomial(c(0, 1))
  zp <- polynom::polynomial(c(0, 0))
  expect_true(is_any_polynomail_nonzero(list(zp, p)))
  expect_false(is_any_polynomail_nonzero(list(zp, zp)))
})


test_that("get_min_degree_non_zero_idx", {
  zp <- polynom::polynomial(c(0))
  p0 <- polynom::polynomial(c(1))
  p1 <- polynom::polynomial(c(1, 1))
  p2 <- polynom::polynomial(c(1, 2, 3))
  expect_equal(get_min_degree_non_zero_idx(list(p0, zp)), 1)
  expect_equal(get_min_degree_non_zero_idx(list(zp, p0)), 2)
  expect_equal(get_min_degree_non_zero_idx(list(zp, p0, p2, p1)), 2)
  expect_equal(get_min_degree_non_zero_idx(list(zp, zp, p2, p1)), 4)
})

test_that("triang_Euclidean", {
  src <- ch2pn(c(
    "s^2", "0",
    "0",   "s^2",
    "1",   "1 + s"), symb="s")
  pm <- polyMgen.d(nrow=3, ncol=2, byrow=TRUE, rawData=src, symb="s")
  result <- triang_Euclidean(pm)
  H_src <- ch2pn(c(
    "1", "1 + s",
    "0",   "s^2",
    "0",   "0"), symb="s")
  H <- polyMgen.d(nrow=3, ncol=2, byrow=TRUE, rawData=H_src, symb="s")
  expect_equal(result$m, H)
  U_src <- ch2pn(c(
    "0",   "0",     "1",
    "0",   "1",     "0",
    "1",   "1 + x", "-x^2"), symb="x")
  U <- polyMgen.d(nrow=3, ncol=3, byrow=TRUE, rawData=U_src, symb="s")
  expect_equal(result$u, U)
})
