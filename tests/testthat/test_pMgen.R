context("pMgen")

test_that("pMgen.a without params", {
  random_matrix <- pMgen.a()
  expect_equal(random_matrix$dim, c(2, 3))
  expect_true(all(random_matrix$degree <= 3))
})

test_that("pMgen.b without params", {
  random_matrix <- pMgen.b()
  expect_equal(random_matrix$dim, c(2, 3))
  expect_true(all(random_matrix$degree <= 3))
})

test_that("pMgen.c without params", {
  random_matrix <- pMgen.c()
  expect_equal(random_matrix$dim, c(2, 3))
  expect_true(all(random_matrix$degree <= 3))
})

test_that("pMgen.d without params", {
  random_matrix <- pMgen.d()
  expect_equal(random_matrix$dim, c(2, 3))
  expect_true(all(random_matrix$degree <= 3))
})

test_that("pMgen without params", {
  random_matrix <- pMgen()
  expect_equal(random_matrix$dim, c(2, 3))
  expect_true(all(random_matrix$degree <= 3))
})
