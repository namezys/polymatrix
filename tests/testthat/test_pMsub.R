context("pMsub")

test_that("pMsub", {
  a <- polyMgen.d(3, 3, rawData=ch2pn(c("-3 + z^2", "2 + 4*z", "-z^2", 
                                        "1", "2", "3 + z", 
                                        "2*z", "0", "2 - 3*z"), "z"), byrow=TRUE, symb="x")
  
  expect_equal(pMsub(a, 1), polyMgen.d(1, 1, rawData=list(ch2pn(c("-3 + z^2"), "z")), byrow=TRUE, symb="x"))
  expect_equal(pMsub(a, 2), polyMgen.d(1, 1, rawData=list(ch2pn(c("2"), "z")), byrow=TRUE, symb="x"))
  expect_equal(pMsub(a, 3), polyMgen.d(1, 1, rawData=list(ch2pn(c("2 - 3*z"), "z")), byrow=TRUE, symb="x"))
  
  b <- polyMgen.d(2, 2, rawData=ch2pn(c("z*5", "2 + z^2",
                                        "z*3", "3 + z"), "z"), byrow=TRUE, symb="x")
  
  expect_equal(pMsub(b, 1), polyMgen.d(1, 1, rawData=list(ch2pn(c("z^5"), "z")), byrow=TRUE, symb="x"))
  expect_equal(pMsub(b, 2), polyMgen.d(1, 1, rawData=list(ch2pn(c("3 + z"), "z")), byrow=TRUE, symb="x"))
})
