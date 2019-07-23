context("proper")

test_that("proper", {
  
  pm1 <- polyMgen.d(2, 2, rawData=ch2pn(c("-1 + 7 * x", "x", "3 - x + x ^ 2", "-1 + x ^ 2 - 3 * x ^ 3")))
  expect_output(proper(pm1, "col"), "\\nThe given matrix is column-proper\\?  TRUE \\n")
  expect_output(proper(pm1, "row"), "\\nThe given matrix is row-proper\\?  FALSE \\n")
  expect_output(proper(pm1, "both"), "\\nThe given matrix is column//row-proper\\?  TRUE FALSE \\n")
  
  pm2 <- polyMgen.d(3, 3, rawData=ch2pn(c("-3", "6", "-3",
                                          "6", "16", "-18", 
                                          "-3", "-10", "9"), "z"), byrow=TRUE, symb="x")
  
  expect_output(proper(pm2, "col"), "\\nThe given matrix is column-proper\\?  TRUE \\n")
  expect_output(proper(pm2, "row"), "\\nThe given matrix is row-proper\\?  TRUE \\n")
  expect_output(proper(pm2, "both"), "\\nThe given matrix is column//row-proper\\?  TRUE TRUE \\n")
})
