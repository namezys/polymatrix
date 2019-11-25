context("triang_euclidean")

p <- polynom::polynomial


test_that("is_polynomail_monic", {
  expect_true(is_polynomail_monic(polynom::polynomial(c(1, 3, 4, 5, 1))))
  expect_true(is_polynomail_monic(polynom::polynomial(c(0, 1))))
  expect_true(is_polynomail_monic(polynom::polynomial(c(1))))

  expect_false(is_polynomail_monic(polynom::polynomial(c(1, 3, 4, 5, 2))))
  expect_false(is_polynomail_monic(polynom::polynomial(c(0, 2))))
  expect_false(is_polynomail_monic(polynom::polynomial(c(3))))
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
    print("result")
    print(result)
    print("expect h")
    print(h)
    print("expect u")
    print(u)
    print("det U")
    print(round(pMdet(result$u), 2))
  }
  expect_equal(round(result$m, 2), round(h, 2))
  expect_equal(round(result$u, 2), round(u, 2))
  expect_true(all(is.zero(result$m - result$u %X% pm)))

  # check if U unimodular
  det = round(pMdet(result$u), 2)
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
