context("pMrow")

test_that("pMrow", {
  a <- polyMgen.d(2, 2, rawData=ch2pn(c("x^2 + 1", "-3", 
                                        "-1*x^2", "-2*x"), "x"), byrow=TRUE, symb="x")
  
  expect_equal(pMrow(a), polyMgen.d(1, 2, rawData=ch2pn(c("x^2 + 1", "-3"), "x"), byrow=TRUE, symb="x"))
  
  b <- polyMgen.d(3, 3, rawData=ch2pn(c("3 + x^2", "1 + 6*x", "-x^2", 
                                        "-1", "3*x^2", "5 + x", 
                                        "3*x", "1", "-3 - 2*x"), "x"), byrow=TRUE, symb="x")
  
  expect_equal(pMrow(b), polyMgen.d(1, 3, rawData=ch2pn(c("3 + x^2", "1 + 6*x", "-x^2"), "x"), byrow=TRUE, symb="x"))
  
})