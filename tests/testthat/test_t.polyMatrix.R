context("t.polyMatrix")

test_that("t.polyMatrix", {
  a <- polyMgen.d(3, 3, rawData=ch2pn(c("-3 + z^2", "2 + 4*z", "-z^2", 
                                        "1", "2", "3 + z", 
                                        "2*z", "0", "2 - 3*z"), "z"), byrow=TRUE, symb="z")
  expect_equal(t.polyMatrix(a), polyMgen.d(3, 3, rawData=ch2pn(c("-3 + z^2", "1", "2*z", 
                                                                 "2 + 4*z", "2", "0", 
                                                                 "-z^2", "3 + z", "2 - 3*z"), "z"), byrow=TRUE, symb="z"))
  
  
  b <- polyMgen.d(2, 2, rawData=ch2pn(c("3 + z^2", "2 + 2*z", "-z^2", 
                                        "1"), "z"), byrow=TRUE, symb="z")
  
  expect_equal(t.polyMatrix(b), polyMgen.d(2, 2, rawData=ch2pn(c("3 + z^2", "-z^2", 
                                                                 "2 + 2*z", "1"), "z"), byrow=TRUE, symb="z"))
})
