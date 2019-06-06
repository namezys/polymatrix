context("pMsgn")

test_that("tr.matrix", {
  
  a <- polyMgen.a(3, 3, c(1, 2, 2, 1, 1), degree=2)
  
  expect_equal(pMsgn(a), polyMgen.a(3, 3, c(-1, -2, -2, -1, -1), degree=2))
  
  
  b <- polyMgen.b(3, 3, c(4, 3, 1, 2, 2), degree=2)
  
  expect_equal(pMsgn(b), polyMgen.b(3, 3, c(-4, -3, -1, -2, -2), degree=2))
  
  
  c <- polyMgen.c(3, 3, c(3, 1, 2, 3, 1), degree=2)
  
  expect_equal(pMsgn(c), polyMgen.c(3, 3, c(-3, -1, -2, -3, -1), degree=2))
  
  
  d <- polyMgen.d(3, 3, rawData=ch2pn(c("-3 + z^2", "2 + 4*z", "-z^2", 
                                         "1", "2", "3 + z", 
                                         "2*z", "0", "2 - 3*z"), "z"), byrow=TRUE, symb="z")
  
  expect_equal(pMsgn(d), polyMgen.d(3, 3, rawData=ch2pn(c("3 - z^2", "-2 - 4*z", "z^2", 
                                                          "-1", "-2", "-3 - z", 
                                                          "-2*z", "0", "-2 + 3*z"), "z"), byrow=TRUE, symb="z"))
  
  
  e <- polyMgen.d(2, 2, rawData=ch2pn(c("3 + z^2", "2 + 2*z", "-z^2", 
                                        "1", "-3", "5 + z"), "z"), byrow=TRUE, symb="z")
  
  expect_equal(pMsgn(e), polyMgen.d(2, 2, rawData=ch2pn(c("-3 - z^2", "-2 - 2*z", "+z^2", 
                                                          "-1", "3", "-5 - z"), "z"), byrow=TRUE, symb="z"))
})
