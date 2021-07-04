context("Methods is.*.proper")

test_that("simple test", {
  init_data()
  expect_true(is.column.proper(pm_first))
  expect_false(is.row.proper(pm_first))
})
