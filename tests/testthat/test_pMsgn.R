context("pMsgn")

test_that("tr.matrix", {
  a <- polyMgen.d(3, 3, rawData=ch2pn(c("-3 + z^2", "2 + 4*z", "-z^2", 
                                         "1", "2", "3 + z", 
                                         "2*z", "0", "2 - 3*z"), "z"), byrow=TRUE, symb="z")
  
  expect_equal(pMsgn(a), polyMgen.d(3, 3, rawData=ch2pn(c("3 - z^2", "-2 - 4*z", "z^2", 
                                                          "-1", "-2", "-3 - z", 
                                                          "-2*z", "0", "-2 + 3*z"), "z"), byrow=TRUE, symb="z"))
  
  b <- polyMgen.d(2, 2, rawData=ch2pn(c("3 + z^2", "2 + 2*z", "-z^2", 
                                        "1", "-3", "5 + z"), "z"), byrow=TRUE, symb="z")
  
  expect_equal(pMsgn(b), polyMgen.d(2, 2, rawData=ch2pn(c("-3 - z^2", "-2 - 2*z", "+z^2", 
                                                          "-1", "3", "-5 - z"), "z"), byrow=TRUE, symb="z"))
})
