context("stirang_sylvester")

M_inv <- polyMgen.d(nrow=3, ncol=2, byrow=TRUE, rawData=ch2pn(c(
  "1    ", "0",
  "s + 2", "s^2",
  "0    ", "0"
), symb="s"), symb="s")

M_inv_ <- polyMgen.d(nrow=3, ncol=5, byrow=TRUE, rawData=ch2pn(c(
  "1    ", "0", "1", "0", "0",
  "s + 2", "s^2", "0", "1", "0",
  "0    ", "0", "0", "0", "1"
), symb="s"), symb="s")


MM <- polyMgen.d(nrow=2, ncol=3, byrow=TRUE, rawData=ch2pn(c(
  "-1.732",             "0"        ,   "       0 ",
  "-0.577*s^2",         "0.408*s^2" , "        0 "
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

test_that("build_sylvester_sub_matrices", {
  expect_equal(
    build_sylvester_sub_matrices(M_1),
    list(
      sub_matrices=list(matrix(c(0, 1, -1, 1, 0, -1), 3, 2), matrix(c(0, 0, 2, 0, 2, 2), 3, 2), matrix(c(0, 0, 0, 0, 0, 3), 3, 2)),
      sub_nrow=3,
      sub_ncol=2
    )
  )

  expect_equal(
    build_sylvester_sub_matrices(M_2),
    list(
      sub_matrices=list(matrix(c(0, 0, 1, -1, 1, 0, 0, -1), 4, 2), matrix(c(0, 0, 0, 2, 0, 0, 2, 2), 4, 2),
                        matrix(c(0, 0, 0, 0, 0, 0, 0, 3), 4, 2)),
      sub_nrow=4,
      sub_ncol=2
    )
  )

  expect_equal(
    build_sylvester_sub_matrices(M_3),
    list(
      sub_matrices=list(matrix(c(0, 1, -1, 1, 0, -1), 3, 2), matrix(c(0, 0, 0, 0, 0, 3), 3, 2)),
      sub_nrow=3,
      sub_ncol=2
    )
  )
})

test_that("build_sylvester_matrix", {
  expect_equal(
    build_sylvester_matrix(M_3, 0),
    matrix(c(
      0, 1,
      1, 0,
      -1, -1,
      0, 0,
      0, 0,
      0, 3
    ), 6, 2, byrow = TRUE)
  )
  expect_equal(
    build_sylvester_matrix(M_3, 1),
    matrix(c(
      0, 1, 0, 0,
      1, 0, 0, 1,
      -1, -1, 1, 0,
      0, 0, -1, -1,
      0, 0, 0, 0,
      0, 0, 0, 0,
      0, 3, 0, 0,
      0, 0, 0, 3
    ), 8, 4, byrow = TRUE)
  )
  expect_equal(
    build_sylvester_matrix(M_3, 2),
    matrix(c(
      0, 1, 0, 0, 0, 0,
      1, 0, 0, 1, 0, 0,
      -1, -1, 1, 0, 0, 1,
      0, 0, -1, -1, 1, 0,
      0, 0, 0, 0, -1, -1,
      0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0,
      0, 3, 0, 0, 0, 0,
      0, 0, 0, 3, 0, 0,
      0, 0, 0, 0, 0, 3
    ), 10, 6, byrow = TRUE)
  )
})

ntest_that <- function(...) {}

sylv_test <- function(pm, u_degree, ch, cu, debug=FALSE)
{
  if (u_degree > 1) {
    for(i in 1:(u_degree - 1)) {
      expect_null(triang_Sylvester(pm, 1))
    }
  }

  result <- triang_Sylvester(pm, u_degree)
  h_src <- ch2pn(ch, symb=pm$symb)
  h <- polyMgen.d(nrow=nrow(pm), ncol=ncol(pm), byrow=TRUE, rawData=h_src, symb=pm$symb)
  u_src <- ch2pn(cu, symb=pm$symb)
  u <- polyMgen.d(nrow=ncol(pm), ncol=ncol(pm), byrow=TRUE, rawData=u_src, symb=pm$symb)
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
    print(round(pMdet(result$U), 2))
    print("calc T")
    print(round(result$T - pm %X% result$U, 4))
  }
  expect_equal(round(result$T, 2), round(h, 2))
  expect_equal(round(result$U, 2), round(u, 2))
  expect_true(all(is.zero(result$T - pm %X% result$U)))

  # check if U unimodular
  det <- round(pMdet(result$U), 2)
  expect_equal(det, polynom::polynomial(coefs(det, 0)))
}

test_that("triang_Sylvester_inv", {
  sylv_test(M_inv, 0, c(
    "-1    ", "0",
    "-2 - s", "s^2",
    "0     ", "0"
  ), c(
    "-1", "0",
    "0 ", "1"
  ))
})

test_that("triang_Sylvester_inv_tr", {
  sylv_test(t(M_inv), 1, c(
    "-1.73    ", "0       ", "0",
    "-0.58*s^2", "0.41*s^2", "0"
  ), c(
    "-0.58 + 0.58*s", "-0.82 - 0.41*s", "0",
    "-0.58         ", "0.41          ", "0",
    "0             ", "0             ", "1"
  ))
})

test_that("triang_Sylvester_case_0", {
  sylv_test(M_0, 0, c(
    "-s^2", "0",
    "0  ", "s^2",
    "-1  ", "s + 1"
  ), c(
    "-1", "0",
    "0 ", "1"
  ))
})

test_that("triang_Sylvester_case_0_tr", {
  sylv_test(t(M_0), 2, c(
    "-1    ", "0            ", "0",
    "-1 - s", "-1.15*s^2", "0"
  ), c(
    "0 ", "0.29          ", "-0.5",
    "0 ", "-0.87 + 0.29*s", "-0.5 - 0.5*s",
    "-1", "-0.29*s^2     ", "0.5*s^2"
  ))
})

test_that("triang_Sylvester_case_1", {
  sylv_test(M_1, 1, c(
    "-1.22 * s + 1.22", "0",
    "-2.45           ", "0",
    "-1.22           ", "1.73"
  ), c(
    "-0.82 + 0.41 * s", "-0.58 * s - 0.58",
    "-0.41           ", "0.58"
  ))
})

test_that("triang_Sylvester_case_1_tr", {
  return()
  sylv_test(t(M_1), 1, c(
    "-2.19         ", "0 ", "0",
    "-2.19 - 2.19*s", "-3", "0"
  ), c(
    "0.37          ", "0 ", "-0.82",
    "-0.91 - 0.18*s", "0 ", "-0.41 + 0.41*s",
    "0             ", "-1", "0"
  ))
})

test_that("triang_Sylvester_case_2", {
  sylv_test(M_2, 2, c(
    "1.15 - 1.15*s   ", "0",
    "-2.31 + 0.58*s^2", "-s^2",
    "-0.87           ", "1.5"
  ), c(
    "-0.8660254 + 0.2886751*s + 0.2886751*s^2", "-0.5 - 0.5*s - 0.5*s^2",
    "-0.2886751                              ",  "0.5"
  ))
})

test_that("triang_Sylvester_case_2_tr", {
  return()
  sylv_test(t(M_2), 3, c(
    "-2.2                                                    ", "0 ", "0",
    "-2.2 - 2.2*s - 0.38*s^2 + 0.31*s^3 + 0.05*s^4 + 0.01*s^5", "-3", "0"
  ), c(
    "0.38 + 0.06*s + 0.01*s^2            ", "0", "-0.76",
    "-0.91 - 0.16*s - 0.03*s^2 - 0.01*s^3", "0", "-0.38 + 0.38*s",
    "0                                   ", "-1", "-0.25*s^2 + 0.25*s^3"
  ))
})

test_that("triang_Sylvester_case_3", {
  sylv_test(M_3, 1, c(
    "1.22 - 1.22*s", "0",
    "-1.22        ", "1.73"
  ), c(
    "-0.82 + 0.41*s", "-0.58 - 0.58*s",
    "-0.41         ", "0.58"
  ))
})

test_that("triang_Sylvester_case_3_tr", {
  sylv_test(t(M_3), 1, c(
    "1 - s  ", "0",
    "1 - s^2", "3"
  ), c(
    "-1", "0",
    "0 ", "1"
  ))
})
