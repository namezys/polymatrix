context("method trace")

test_that("matrix", {
  init_data()

  expect_equal(tr(m_first), 12)
})

test_that("polynomial", {
  expect_equal(p(1, 2, 3, 4), p(1, 2, 3, 4))
})

test_that("polyMatrix", {
  init_data()

  expect_equal(tr(pm_first), p(4, 1, 2))
  expect_equal(tr(pm_second), 0)
  expect_equal(tr(pm_third), p(3, 9, 4))
  expect_equal(tr(pm_fourth), p(4, 2, 1))
})