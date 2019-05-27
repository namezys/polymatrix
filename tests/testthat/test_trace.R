context("trace")

test_that("tr.matrix", {
  a <- matrix(1:18, 3, 6)
  # 1  4  7  10  13  16
  # 2  5  8  11  14  17
  # 3  6  9  12  15  18
  
  expect_equal(tr.matrix(a), 15)
  
  b <- matrix(3:11, 3, 3)
  # 3  6  9
  # 4  7  10
  # 5  8  11
  
  expect_equal(tr.matrix(b), 21)
  
  c <- matrix(30:41, 6, 2)
  # 30  36
  # 31  37
  # 32  38
  # 33  39
  # 34  40
  # 35  41
  
  expect_equal(tr.matrix(c), 67)
})

test_that("tr.polyMatrix", {
  a <- polyMgen.d(3, 3, rawData=ch2pn(c("-3 + z^2", "2 + 4*z", "-z^2", 
                                        "1", "2", "3 + z", 
                                        "2*z", "0", "2 - 3*z"), "z"), byrow=TRUE, symb="z")
  expect_equal(tr.polyMatrix(a), polynomial(c(1, -3, 1)))
  
  b <- polyMgen.d(2, 2, rawData=ch2pn(c("3 + z^2", "2 + 2*z", "-z^2", 
                                        "1", "-3", "5 + z"), "z"), byrow=TRUE, symb="z")
  expect_equal(tr.polyMatrix(b), polynomial(c(4, 0, 1)))
  
  
})