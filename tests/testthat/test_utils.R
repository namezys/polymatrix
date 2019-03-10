context("utils")

test_that("is.pMatrix", {
  expect_true(is.pMatrix.pMarray(pMgen.a()))
  expect_false(is.pMatrix.pMbroad(pMgen.a()))
  expect_false(is.pMatrix.pMcells(pMgen.a()))
  expect_false(is.pMatrix.pMdlist(pMgen.a()))
  expect_true(is.pMatrix(pMgen.a()))
  
  expect_false(is.pMatrix.pMarray(pMgen.b()))
  expect_true(is.pMatrix.pMbroad(pMgen.b()))
  expect_false(is.pMatrix.pMcells(pMgen.b()))
  expect_false(is.pMatrix.pMdlist(pMgen.b()))
  expect_true(is.pMatrix(pMgen.b()))
  
  expect_false(is.pMatrix.pMarray(pMgen.c()))
  expect_false(is.pMatrix.pMbroad(pMgen.c()))
  expect_true(is.pMatrix.pMcells(pMgen.c()))
  expect_false(is.pMatrix.pMdlist(pMgen.c()))
  expect_true(is.pMatrix(pMgen.c()))
  
  expect_false(is.pMatrix.pMarray(pMgen.d()))
  expect_false(is.pMatrix.pMbroad(pMgen.d()))
  expect_false(is.pMatrix.pMcells(pMgen.d()))
  expect_true(is.pMatrix.pMdlist(pMgen.d()))
  expect_true(is.pMatrix(pMgen.d()))
  
  expect_false(is.pMatrix.pMarray(1))
  expect_false(is.pMatrix.pMbroad(1))
  expect_false(is.pMatrix.pMcells(1))
  expect_false(is.pMatrix.pMdlist(1))
  expect_false(is.pMatrix(1))
  
  expect_false(is.pMatrix.pMarray(""))
  expect_false(is.pMatrix.pMbroad(""))
  expect_false(is.pMatrix.pMcells(""))
  expect_false(is.pMatrix.pMdlist(""))
  expect_false(is.pMatrix("pMgen.a()"))
})