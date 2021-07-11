context("method det")

test_that("polyMatrix", {
  init_data()

  expect_equal(det(pm_first), p(2, -19, 3, -12, 50, 18))
  expect_equal(det(pm_second), p(0))
})

test_that("minor", {
  init_data()

  expect_equal(minor(pm_first, 1, 2), p(0, 3, 1))
  expect_equal(minor(pm_first, 1, 1), p(2))
})