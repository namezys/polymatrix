context("pVsk")

test_that("pVsk", {
  a <- polyMgen.d(3, 3, rawData=ch2pn(c("-3 + z^2", "2 + 4*z", "-z^2", 
                                        "1", "2", "3 + z", 
                                        "2*z", "0", "2 - 3*z"), "z"), byrow=TRUE, symb="z")
  
  expect_equal(pVsk(pMcol(a, 1)), polynomial(c(10, 0, -2, 0, 1)))
  
  b <- polyMgen.d(2, 2, rawData=ch2pn(c("3 + z^2", "2 + 2*z", "-z^2", 
                                        "1", "-3", "5 + z"), "z"), byrow=TRUE, symb="z")
  
  expect_equal(pVsk(pMcol(b, 1)), polynomial(c(9, 0, 6, 0, 2)))
})
