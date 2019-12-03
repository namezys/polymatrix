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
    "-1.732051     ", "0            ", "0",
    "-0.5773503*s^2", "0.4082483*s^2", "0"
  ), c(
    "-0.5773503 + 0.5773503*s", "-0.8164966 - 0.4082483*s", "0",
    "-0.5773503              ", "0.4082483               ", "0",
    "0                       ", "0                       ", "1"
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
    "-1 - s", "-1.154701*s^2", "0"
  ), c(
    "0 ", "0.2886751               ", "-0.5",
    "0 ", "-0.8660254 + 0.2886751*s", "-0.5 - 0.5*s",
    "-1", "-0.2886751*s^2          ", "0.5*s^2"
  ))
})

test_that("triang_Sylvester_case_1", {
  sylv_test(M_1, 1, c(
    "-1.2247 * s + 1.2247", "0",
    "-2.449              ", "0",
    "-1.2247             ", "1.732"
  ), c(
    "-0.816 + 0.408 * s", "-0.577 * s - 0.577",
    "-0.408            ", "0.577"
  ))
})

test_that("triang_Sylvester_case_1_tr", {
  sylv_test(t(M_1), 1, c(
    "-2.19089            ", "0 ", "0",
    "-2.19089 - 2.19089*s", "-3", "0"
  ), c(
    "0.3651484               ", "0 ", "-0.8164966              ",
    "-0.9128709 - 0.1825742*s", "0 ", "-0.4082483 + 0.4082483*s",
    "0                       ", "-1", "0"
  ))
})

test_that("triang_Sylvester_case_2", {
  sylv_test(M_2, 2, c(
    "1.154701 - 1.154701*s    ", "0",
    "-2.309401 + 0.5773503*s^2", "-s^2",
    "-0.8660254               ", "1.5"

  ), c(
    "-0.8660254 + 0.2886751*s + 0.2886751*s^2", "-0.5 - 0.5*s - 0.5*s^2",
    "-0.2886751                              ",  "0.5"
  ))
})

test_that("triang_Sylvester_case_2_tr", {
  sylv_test(t(M_2), 3, c(
    "-2.197363                                                                               ", "0 ", "0",
    "-2.197363 - 2.197363*s - 0.3769985*s^2 + 0.3123702*s^3 + 0.05385693*s^4 + 0.01077139*s^5", "-3", "0"
  ), c(
    "0.3769985 + 0.06462831*s + 0.01077139*s^2                  ", "0", "-0.7620008",
    "-0.9101821 - 0.1561851*s - 0.02692846*s^2 - 0.005385693*s^3", "0", "-0.3810004 + 0.3810004*s",
    "0                                                          ", "-1", "-0.2540003*s^2 + 0.2540003*s^3"
  ))
})

test_that("triang_Sylvester_case_3", {
  sylv_test(M_3, 1, c(
    "1.224745 - 1.224745*s", "0",
    "-1.224745            ", "1.732051"
  ), c(
    "-0.8164966 + 0.4082483*s", "-0.5773503 - 0.5773503*s",
    "-0.4082483              ", "0.5773503"
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
