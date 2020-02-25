context("triang_euclidean")

p <- polynom::polynomial


test_that("is_polynomial_monic", {
  expect_true(is_polynomial_monic(polynom::polynomial(c(1, 3, 4, 5, 1))))
  expect_true(is_polynomial_monic(polynom::polynomial(c(0, 1))))
  expect_true(is_polynomial_monic(polynom::polynomial(c(1))))

  expect_false(is_polynomial_monic(polynom::polynomial(c(1, 3, 4, 5, 2))))
  expect_false(is_polynomial_monic(polynom::polynomial(c(0, 2))))
  expect_false(is_polynomial_monic(polynom::polynomial(c(3))))
})

test_that("is_any_polynomial_nonzero", {
  p <- polynom::polynomial(c(0, 1))
  zp <- polynom::polynomial(c(0, 0))
  expect_true(is_any_polynomial_nonzero(list(zp, p)))
  expect_false(is_any_polynomial_nonzero(list(zp, zp)))
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

M_inv <- polyMgen.d(nrow=3, ncol=2, byrow=TRUE, rawData=ch2pn(c(
  "1", "s + 2",
  "0", "s^2",
  "0", "0"
  ), symb="s"), symb="s")

M_0 <- polyMgen.d(nrow=3, ncol=2, byrow=TRUE, rawData=ch2pn(c(
  "s^2", "0",
  "0  ", "s^2",
  "1  ", "s + 1"
), symb="s"), symb="s")

M_1 <- polyMgen.d(nrow=3, ncol=2, byrow=TRUE, ch2pn(c(
  "s - 1", "s^2 - 1",
  "2    ", "2*s + 2",
  "0    ", "3      "
  ), symb="s"), symb="s")

M_2 <- polyMgen.d(nrow=3, ncol=2, byrow=TRUE, ch2pn(c(
  "s - 1", "s^3 - 1",
  "2    ", "2*s + 2",
  "0    ", "3      "
  ), symb="s"), symb="s")

M_3 <- polyMgen.d(nrow=2, ncol=2, byrow=TRUE, ch2pn(c(
  "s - 1", "s^2 - 1",
  "0    ", "3"
  ), symb="s"), symb="s")


M_4 <- polyMgen.d(nrow=3,ncol=3,byrow=TRUE,rawData=ch2pn(c(
  "4 + 4*s + s^2          ", "2*s + s^2              ", "2 + 3*s + s^2",
  "6 + 5*s + 3*s^2 + s^3  ", "3 + 3*s + 2*s^2 + s^3  ", "3 + 4*s + 2*s^2 + s^3",
  "9 + 6*s + 3*s^2 + 2*s^3", "5 + 4*s + 2*s^2 + 2*s^3", "5 + 5*s + 2*s^2 + 2*s^3"
  ),symb="s"),symb="s")


ntest_that <- function(...) {}

euc_test <- function(pm, ch, cu, debug=FALSE)
{
  result <- triang_Euclidean(pm)
  h_src <- ch2pn(ch, symb=pm$symb)
  h <- polyMgen.d(nrow=nrow(pm), ncol=ncol(pm), byrow=TRUE, rawData=h_src, symb=pm$symb)
  u_src <- ch2pn(cu, symb=pm$symb)
  u <- polyMgen.d(nrow=nrow(pm), ncol=nrow(pm), byrow=TRUE, rawData=u_src, symb=pm$symb)
  if (debug) {
    print("debug")
    print("src")
    print(pm)
    print("result T")
    print(round(result$T, 2))
    print("result U")
    print(round(result$U, 2))
    print("expect h")
    print(h)
    print("expect u")
    print(u)
    print("det U")
    print(round(pMdet(result$U), 2))
  }
  expect_equal(round(result$T, 2), round(h, 2))
  expect_equal(round(result$U, 2), round(u, 2))
  expect_true(all(is.zero(result$T - result$U %X% pm)))

  # check if U unimodular
  det = round(pMdet(result$U), 2)
  expect_equal(det, polynom::polynomial(coefs(det, 0)))
}

test_that("triang_Euclidean_case_inv", {
  euc_test(
    M_inv, c(
      "1", "s + 2",
      "0", "s^2",
      "0", "0"
    ), c(
      "1", "0", "0",
      "0", "1", "0",
      "0", "0", "1"
    ))
})

test_that("triang_Euclidean_case_inv_mult", {
  euc_test(
    3 %X% M_inv, c(
      "1", "s + 2",
      "0", "s^2",
      "0", "0"
    ), c(
      "0.333", "0", "0",
      "0", "0.333", "0",
      "0", "0", "1"
    ))
})

test_that("triang_Euclidean_case_0", {
  euc_test(
    M_0, c(
      "1", "1 + s",
      "0", "s^2",
      "0", "0"
    ), c(
      "0", "0    ", "1",
      "0", "1    ", "0",
      "1", "1 + s", "-s^2"
    ))
})

test_that("triang_Euclidean_case_0_tr", {
  euc_test(
    t(M_0), c(
      "s^2", "0  ", "1",
      "0  ", "s^2", "1 + s"
    ), c(
      "1", "0",
      "0", "1"
    ))
})

test_that("triang_Euclidean_case_1", {
  euc_test(
    M_1, c(
      "1", "0",
      "0", "1",
      "0", "0"
    ), c(
      "0", "0.5        ", "-0.3333333 - 0.3333333*s",
      "0", "0          ", "0.3333333",
      "1", "0.5 - 0.5*s", "0"
    ))
})

test_that("triang_Euclidean_case_1_tr", {
  # stop becuase no hermit form t(M_1)
})

test_that("triang_Euclidean_case_2", {
  euc_test(
    M_2, c(
      "1", "0",
      "0", "1",
      "0", "0"
    ), c(
      "0", "0.5        ", "-0.3333333 - 0.3333333*s",
      "0", "0          ", "0.3333333",
      "1", "0.5 - 0.5*s", " 0.3333333*s^2 - 0.3333333*s^3"
    ))
})

test_that("triang_Euclidean_case_2_tr", {
  euc_test(
    t(M_2), c(
      "-1 + s", "  2", "0",
      "0     ", "s^2", "-1.5"
    ), c(
      "1                    ", "0",
      "0.5 + 0.5*s + 0.5*s^2", "-0.5"
    ))
})

test_that("triang_Euclidean_case_3", {
  euc_test(
    M_3, c(
      "-1 + s", "0",
      "0     ", "1"
    ), c(
      "1", "0.3333333 - 0.3333333*s^2",
      "0", "0.3333333"
    ))
})

test_that("triang_Euclidean_case_3_tr", {
  euc_test(
    t(M_3), c(
      "-1 + s", "0",
      "0     ", "1"
    ), c(
      "1                       ", "0",
      "-0.3333333 - 0.3333333*s", "0.3333333"
    ))
})


test_that("triang_Euclidean_case_4", {
  return()
  euc_test(
    M_4, c(
      "1", "0", "0.423 + 0.229*s - 0.168*s^2 - 0.006*s^3 - 0.06*s^4 - 0.03*s^5",
      "0", "1", "-0.03 + 0.05*s - 0.02*s^2 - 0.03*s^3 + 0.05*s^4 - 0.02*s^5",
      "0", "0", "6 + 3*s + 2*s^2 + 7*s^3 + 3*s^4 + 2*s^5 + s^6"
    ), c(
      "0.21 - 0.04*s - 0.01*s^2 - 0.06*s^3 - 0.03*s^4", "0.26 + 0.01*s + 0.22*s^2 + 0.08*s^3", "-0.15 - 0.12*s - 0.11*s^2 - 0.03*s^3",
      "-0.51 + 0.1*s - 0.03*s^2 + 0.05*s^3 - 0.02*s^4", "0.43 - 0.14*s - 0.12*s^2 + 0.07*s^3", "-0.06 + 0.08*s + 0.01*s^2 - 0.02*s^3",
      "3 + 4*s + 2*s^2 + 3*s^3 + 2*s^4 + s^5         ", "-20 - 18*s - 8*s^2 - 8*s^3 - 3*s^4 ", "12 + 12*s + 7*s^2 + 4*s^3 + s^4"
    ))
})
