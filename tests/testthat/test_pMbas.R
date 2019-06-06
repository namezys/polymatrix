context("pMbas")

test_that("pMbas", {
  a <- polyMgen.d(3, 3, rawData=ch2pn(c("-3 + z^2", "2 + 4*z", "-z^2", 
                                        "1", "2", "3 + z", 
                                        "2*z", "0", "2 - 3*z"), "z"), byrow=TRUE, symb="x")
  
  expect_equal(pMbas(a, c(2, 3, 1), byrow=TRUE), polyMgen.d(3, 1, rawData=ch2pn(c("2 + 4*z",
                                                                                  "3 + z", 
                                                                                  "2*z"), "z"), byrow=TRUE, symb="x"))
})
