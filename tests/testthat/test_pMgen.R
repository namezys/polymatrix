context("polyMgen")

test_that("polyMgen.a without params", {
  random_matrix <- polyMgen.a()
  expect_equal(random_matrix$dim, c(2, 3))
  expect_true(all(random_matrix$degree <= 3))
})

test_that("polyMgen.b without params", {
  random_matrix <- polyMgen.b()
  expect_equal(random_matrix$dim, c(2, 3))
  expect_true(all(random_matrix$degree <= 3))
})

test_that("polyMgen.c without params", {
  random_matrix <- polyMgen.c()
  expect_equal(random_matrix$dim, c(2, 3))
  expect_true(all(random_matrix$degree <= 3))
})

test_that("polyMgen.d without params", {
  random_matrix <- polyMgen.d()
  expect_equal(random_matrix$dim, c(2, 3))
  expect_true(all(random_matrix$degree <= 3))
})

test_that("polyMgen.d with numerical data", {
  m = polyMgen.d(2, 3, c(1, 2, 3, 10, 11))
  expect_equal(m$dim, c(2, 3))
  expect_equal(m$degree, matrix(0, 2, 3))
  expect_equal(m$dlist[[1]][[1]], polynomial(1))
  expect_equal(m$dlist[[2]][[1]], polynomial(2))
  expect_equal(m$dlist[[1]][[2]], polynomial(3))
  expect_equal(m$dlist[[2]][[2]], polynomial(10))
  expect_equal(m$dlist[[1]][[3]], polynomial(11))
  expect_equal(m$dlist[[2]][[3]], polynomial(1))
})

test_that("polyMgen.d with polynomail data", {
  m = polyMgen.d(2, 3, list(polynomial(c(1, 2)), polynomial(c(3, 4)), polynomial(c(2,3,4))))
  expect_equal(m$dim, c(2, 3))
  expect_equal(m$degree, matrix(c(1, 1, 2, 1, 1, 2), 2, 3))
  expect_equal(m$dlist[[1]][[1]], polynomial(c(1, 2)))
  expect_equal(m$dlist[[2]][[1]], polynomial(c(3, 4)))
  expect_equal(m$dlist[[1]][[2]], polynomial(c(2, 3, 4)))
})

test_that("polyMgen without params", {
  random_matrix <- polyMgen()
  expect_equal(random_matrix$dim, c(2, 3))
  expect_true(all(random_matrix$degree <= 3))
})
