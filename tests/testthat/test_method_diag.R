context("method diag")

test_that("from polynomial", {
  v <- p(1, 2, -1)
  pm2x2 <- parse.polyMatrix(
    "1 + 2x - x^2,            0",
    "           0, 1 + 2x - x^2"
  )
  expect_equal(diag(v, 2, 2), pm2x2)
  expect_equal(diag(v, 2), pm2x2)
  expect_equal(diag(v, ncol=2), pm2x2)
})

test_that("from polyMatrix", {
  init_data()

  expect_equal(diag(pm_first),
               parse.polyMatrix(
                 "1 + x + 2x^2, 2, 1"
               ))
  expect_equal(diag(pm_second), polyMatrix(0, 1, 3))
})
