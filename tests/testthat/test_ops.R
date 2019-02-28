context("Ops")

test_that("equal", {
  f = pMgen.a(2, 3, c(1, 1, 1, 1, 1), degree=0)
  s = pMgen.a(2, 2, c(1, 1, 1, 1, 1), degree=0)
  expect_true(f == f)
  expect_false(f == s)
  expect_true(f != s)
  expect_false(f != f)
})
