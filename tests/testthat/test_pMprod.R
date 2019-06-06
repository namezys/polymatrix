context("pMprod")

test_that("pMprod", {
  set.seed(22)
  a <- polyMgen.a(1, 2)
  
  expect_equal(pMprod(a), polynomial(c(4, 13, 22, 15)))
  
  
  set.seed(13)
  b <- polyMgen.a(1, 2)
  
  expect_equal(pMprod(b), polynomial(c(5, 16, 34, 52, 45, 28)))
})