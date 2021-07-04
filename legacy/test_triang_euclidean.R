context("triang_euclidean")

M_inv <- parse.polyMatrix("1, x + 2",
                          "0, x^2",
                          "0, 0")
M_0 <- parse.polyMatrix("x^2, 0",
                        "0  , x^2",
                        "1  , x + 1")
M_1 <- parse.polyMatrix("x - 1, x^2 - 1",
                        "2    , 2x + 2",
                        "0    , 3      ")
M_2 <- parse.polyMatrix("x - 1, x^3 - 1",
                        "2    , 2*x + 2",
                        "0    , 3      ")
M_3 <- parse.polyMatrix("x - 1, x^2 - 1",
                        "0    , 3")
M_4 <- parse.polyMatrix("4 + 4x + x^2        , 2x + x^2            , 2 + 3x + x^2",
                        "6 + 5x + 3x^2 + x^3 , 3 + 3x + 2x^2 + x^3 , 3 + 4x + 2x^2 + x^3",
                        "9 + 6x + 3x^2 + 2x^3, 5 + 4x + 2x^2 + 2x^3, 5 + 5x + 2x^2 + 2x^3")
euc_test <- function(pm, upper_tr_from, transformation_matrix, debug=FALSE)
{
  result <- triang.euclidean(pm)
  if (debug) {
    print("debug")
    print("src")
    print(pm)
    print("result R")
    print(round(result$R, 2))
    print("result Q")
    print(round(result$Q, 2))
    print("expect R")
    print(upper_tr_from)
    print("expect Q")
    print(transformation_matrix)
    #print("det U")
    #print(round(pMdet(result$U), 2))
  }
  expect_equal(round(result$R, 2), round(upper_tr_from, 2))
  expect_equal(round(result$Q, 2), round(transformation_matrix, 2))
  expect_true(all(is.zero(result$R - result$Q %x% pm)))

  # check if U unimodular
  #det <- round(pMdet(result$U), 2)
  #expect_equal(det, polynom::polynomial(coefs(det, 0)))
}
#
test_that("triang_Euclidean_case_inv", {
  euc_test(M_inv,
           parse.polyMatrix("1, x + 2",
                            "0, x^2",
                            "0", "0"),
           parse.polyMatrix("1, 0, 0",
                            "0, 1, 0",
                            "0, 0, 1")
  )
})
#
#test_that("triang_Euclidean_case_inv_mult", {
#  euc_test(
#    3 %X% M_inv, c(
#      "1", "s + 2",
#      "0", "s^2",
#      "0", "0"
#    ), c(
#      "0.333", "0", "0",
#      "0", "0.333", "0",
#      "0", "0", "1"
#    ))
#})
#
#test_that("triang_Euclidean_case_0", {
#  euc_test(
#    M_0, c(
#      "1", "1 + s",
#      "0", "s^2",
#      "0", "0"
#    ), c(
#      "0", "0    ", "1",
#      "0", "1    ", "0",
#      "1", "1 + s", "-s^2"
#    ))
#})
#
#test_that("triang_Euclidean_case_0_tr", {
#  euc_test(
#    t(M_0), c(
#      "s^2", "0  ", "1",
#      "0  ", "s^2", "1 + s"
#    ), c(
#      "1", "0",
#      "0", "1"
#    ))
#})
#
#test_that("triang_Euclidean_case_1", {
#  euc_test(
#    M_1, c(
#      "1", "0",
#      "0", "1",
#      "0", "0"
#    ), c(
#      "0", "0.5        ", "-0.3333333 - 0.3333333*s",
#      "0", "0          ", "0.3333333",
#      "1", "0.5 - 0.5*s", "0"
#    ))
#})
#
#test_that("triang_Euclidean_case_1_tr", {
#  # stop becuase no hermit form t(M_1)
#})
#
#test_that("triang_Euclidean_case_2", {
#  euc_test(
#    M_2, c(
#      "1", "0",
#      "0", "1",
#      "0", "0"
#    ), c(
#      "0", "0.5        ", "-0.3333333 - 0.3333333*s",
#      "0", "0          ", "0.3333333",
#      "1", "0.5 - 0.5*s", " 0.3333333*s^2 - 0.3333333*s^3"
#    ))
#})
#
#test_that("triang_Euclidean_case_2_tr", {
#  euc_test(
#    t(M_2), c(
#      "-1 + s", "  2", "0",
#      "0     ", "s^2", "-1.5"
#    ), c(
#      "1                    ", "0",
#      "0.5 + 0.5*s + 0.5*s^2", "-0.5"
#    ))
#})
#
#test_that("triang_Euclidean_case_3", {
#  euc_test(
#    M_3, c(
#      "-1 + s", "0",
#      "0     ", "1"
#    ), c(
#      "1", "0.3333333 - 0.3333333*s^2",
#      "0", "0.3333333"
#    ))
#})
#
#test_that("triang_Euclidean_case_3_tr", {
#  euc_test(
#    t(M_3), c(
#      "-1 + s", "0",
#      "0     ", "1"
#    ), c(
#      "1                       ", "0",
#      "-0.3333333 - 0.3333333*s", "0.3333333"
#    ))
#})
#
#
#test_that("triang_Euclidean_case_4", {
#  return()
#  euc_test(
#    M_4, c(
#      "1", "0", "0.423 + 0.229*s - 0.168*s^2 - 0.006*s^3 - 0.06*s^4 - 0.03*s^5",
#      "0", "1", "-0.03 + 0.05*s - 0.02*s^2 - 0.03*s^3 + 0.05*s^4 - 0.02*s^5",
#      "0", "0", "6 + 3*s + 2*s^2 + 7*s^3 + 3*s^4 + 2*s^5 + s^6"
#    ), c(
#      "0.21 - 0.04*s - 0.01*s^2 - 0.06*s^3 - 0.03*s^4", "0.26 + 0.01*s + 0.22*s^2 + 0.08*s^3", "-0.15 - 0.12*s - 0.11*s^2 - 0.03*s^3",
#      "-0.51 + 0.1*s - 0.03*s^2 + 0.05*s^3 - 0.02*s^4", "0.43 - 0.14*s - 0.12*s^2 + 0.07*s^3", "-0.06 + 0.08*s + 0.01*s^2 - 0.02*s^3",
#      "3 + 4*s + 2*s^2 + 3*s^3 + 2*s^4 + s^5         ", "-20 - 18*s - 8*s^2 - 8*s^3 - 3*s^4 ", "12 + 12*s + 7*s^2 + 4*s^3 + s^4"
#    ))
#})
