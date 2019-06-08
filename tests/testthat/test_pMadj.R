context("pMadj")

test_that("pMadj", {
  a <- matrix(c(3, 0:7), ncol=3)
  # 3   2   5
  # 0   3   6
  # 1   4   7
  
  expect_equal(pMadj(M2pM(a)), polyMgen.d(3, 3, rawData=ch2pn(c("-3", "6", "-3", 
                                                                "6", "16", "-18", 
                                                                "-3", "-10", "9"), "z"), byrow=TRUE, symb="x"))
  
  b <- polyMgen.d(3, 3, rawData=ch2pn(c("-3 + z^2", "2 + 4*z", "-z^2", 
                                        "1", "2", "3 + z", 
                                        "2*z", "0", "2 - 3*z"), "z"), byrow=TRUE, symb="z")
  
  expect_equal(pMadj(b), polyMgen.d(3, 3, rawData=ch2pn(c("4 - 6*z", "-4 - 2*z + 12*z^2", "6 + 14*z + 6*z^2", 
                                                          "-2 + 9*z + 2*z^2", "-6 + 9*z + 2*z^2 - z^3", "9 + 3*z - 4*z^2 - z^3", 
                                                          "-4*z", "4*z + 8*z^2", "-8 - 4*z + 2*z^2"), "z"), byrow=TRUE, symb="z"))
})

