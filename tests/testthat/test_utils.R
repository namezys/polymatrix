context("utils")

p <- polynom::polynomial

test_that("is.polyMatrix", {
  expect_true(is.polyMatrix.polyMarray(polyMgen.a()))
  expect_false(is.polyMatrix.polyMbroad(polyMgen.a()))
  expect_false(is.polyMatrix.polyMcells(polyMgen.a()))
  expect_false(is.polyMatrix.polyMdlist(polyMgen.a()))
  expect_true(is.polyMatrix(polyMgen.a()))

  expect_false(is.polyMatrix.polyMarray(polyMgen.b()))
  expect_true(is.polyMatrix.polyMbroad(polyMgen.b()))
  expect_false(is.polyMatrix.polyMcells(polyMgen.b()))
  expect_false(is.polyMatrix.polyMdlist(polyMgen.b()))
  expect_true(is.polyMatrix(polyMgen.b()))

  expect_false(is.polyMatrix.polyMarray(polyMgen.c()))
  expect_false(is.polyMatrix.polyMbroad(polyMgen.c()))
  expect_true(is.polyMatrix.polyMcells(polyMgen.c()))
  expect_false(is.polyMatrix.polyMdlist(polyMgen.c()))
  expect_true(is.polyMatrix(polyMgen.c()))

  expect_false(is.polyMatrix.polyMarray(polyMgen.d()))
  expect_false(is.polyMatrix.polyMbroad(polyMgen.d()))
  expect_false(is.polyMatrix.polyMcells(polyMgen.d()))
  expect_true(is.polyMatrix.polyMdlist(polyMgen.d()))
  expect_true(is.polyMatrix(polyMgen.d()))

  expect_false(is.polyMatrix.polyMarray(1))
  expect_false(is.polyMatrix.polyMbroad(1))
  expect_false(is.polyMatrix.polyMcells(1))
  expect_false(is.polyMatrix.polyMdlist(1))
  expect_false(is.polyMatrix(1))

  expect_false(is.polyMatrix.polyMarray(""))
  expect_false(is.polyMatrix.polyMbroad(""))
  expect_false(is.polyMatrix.polyMcells(""))
  expect_false(is.polyMatrix.polyMdlist(""))
  expect_false(is.polyMatrix("polyMgen.a()"))
})

test_that("GCD/LCM", {
  a <- polyMgen.d(3, 2, list(p(c(1, 3)),  2, p(c(2, 3)), 3, p(c(1, 3))))
  expect_equal(GCD.polyMatrix(a), p(1))
  expect_equal(LCM.polyMatrix(a), p(c(2, 9, 9)))

  b <- polyMgen.d(3, 2, list(p(c(1, 3)),  2, p(c(1, 3)), 3, p(c(1, 3))))
  expect_equal(GCD.polyMatrix(b), p(1))
  expect_equal(LCM.polyMatrix(b), p(c(3, 9)))

  c <- polyMgen.d(3, 2, list(p(c(1, 3)),  p(c(1, 6, 9)), p(c(1, 0, -9))))
  expect_equal(GCD.polyMatrix(c), p(c(1, 3)))
  expect_equal(LCM.polyMatrix(c), p(c(1, 3, -9, -27)) / 2)
})
