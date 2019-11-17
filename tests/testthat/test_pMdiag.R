context("pMdiag")

test_that("pMdiag", {

  a <- c("x","2*x","3*x","4*x")
  expect_equal(pMdiag(ch2pn(a), 3), polyMgen.d(3, 3, rawData=ch2pn(c("z", "0", "0",
                                                                     "0", "2*z", "0",
                                                                     "0", "0", "3*z"), "z"), byrow=TRUE, symb="x"))


  b <- c("1+2*x+3*x^2")
  expect_equal(pMdiag(ch2pn(b), 3), polyMgen.d(3, 3, rawData=ch2pn(c("1 + 2*z + 3*z^2", "0", "0",
                                                                     "0", "1 + 2*z + 3*z^2", "0",
                                                                     "0", "0", "1 + 2*z + 3*z^2"), "z"), byrow=TRUE, symb="x"))

})
