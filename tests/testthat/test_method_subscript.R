context("method subscript")

test_that("get by index", {
  init_data()

  expect_equal(pm_first[1, 1], p(1, 1, 2))
  expect_equal(pm_first[2, 1], p(0, 3, 1))
  expect_equal(pm_first[1, 2], p(1, 0, 4))
  expect_equal(pm_first[2, 2], 2)

  expect_equal(m_first[1, 1], 3)
  expect_equal(m_first[2, 1], 7)
  expect_equal(m_first[1, 2], 0)
  expect_equal(m_first[2, 2], 1)
})

test_that("get all", {
  init_data()

  expect_equal(pm_first[,], pm_first)
  expect_equal(m_first[,], m_first)
})

test_that("get by sequnce", {
  init_data()

  expect_equal(pm_first[c(1, 3), c(1, 3)],
               polyMatrix(matrix(c(1, 0, 1, 3, 2, 0,
                                   3, 1, 0, 0, 0, 0), 2, 6, byrow = TRUE),
                          2, 2, 2))
  expect_equal(pm_first[c(3, 1), c(3, 1)],
               polyMatrix(matrix(c(1, 3, 0, 0, 0, 0,
                                   0, 1, 3, 1, 0, 2), 2, 6, byrow = TRUE),
                          2, 2, 2))
  expect_equal(pm_first[c(3, 1, 3), c(3, 1, 3)],
               polyMatrix(matrix(c(1, 3, 1, 0, 0, 0, 0, 0, 0,
                                   0, 1, 0, 3, 1, 3, 0, 2, 0,
                                   1, 3, 1, 0, 0, 0, 0, 0, 0), 3, 9, byrow = TRUE),
                          3, 3, 2))
  expect_equal(pm_first[c(3, 1), 1],
               polyMatrix(matrix(c(3, 0, 0,
                                   1, 1, 2), 2, 3, byrow = TRUE),
                          2, 1, 2))
  expect_equal(pm_first[1, c(3, 1)],
               polyMatrix(matrix(c(0, 1, 3, 1, 0, 2), 1, 6), 1, 2, 2))

  expect_equal(m_first[c(1, 3), c(1, 3)],
               matrix(c(3, 2, 4, 8), 2, 2, byrow = TRUE))
  expect_equal(m_first[c(3, 1), c(3, 1)],
               matrix(c(8, 4, 2, 3), 2, 2, byrow = TRUE))
  expect_equal(m_first[c(3, 1, 3), c(3, 1, 3)],
               matrix(c(8, 4, 8, 2, 3, 2, 8, 4, 8), 3, 3, byrow = TRUE))
  expect_equal(m_first[c(3, 1), 1], c(4, 3))
  expect_equal(m_first[1, c(3, 1)], c(2, 3))
})

test_that("get by logic", {
  init_data()

  expect_equal(pm_first[c(TRUE, FALSE, FALSE), c(TRUE, FALSE, FALSE)], p(1, 1, 2))
  expect_equal(pm_first[c(FALSE, TRUE, FALSE), c(TRUE, FALSE, FALSE)], p(0, 3, 1))
  expect_equal(pm_first[c(TRUE, FALSE, FALSE), c(FALSE, TRUE, FALSE)], p(1, 0, 4))
  expect_equal(pm_first[c(FALSE, TRUE, FALSE), c(FALSE, TRUE, FALSE)], 2)


  expect_equal(pm_first[c(TRUE, FALSE), c(TRUE, FALSE)],
               polyMatrix(matrix(c(1, 0, 1, 3, 2, 0,
                                   3, 1, 0, 0, 0, 0), 2, 6, byrow = TRUE),
                          2, 2, 2))
  expect_equal(pm_first[c(FALSE, TRUE), c(TRUE, FALSE)],
               polyMatrix(matrix(c(0, 0, 3, 0, 1, 0), 1, 6, byrow = TRUE),
                          1, 2, 2))
  expect_equal(pm_first[c(TRUE, FALSE), c(FALSE, TRUE)],
               polyMatrix(matrix(c(1, 0, 4,
                                   0, 0, 6), 2, 3, byrow = TRUE),
                          2, 1, 2))
  expect_equal(pm_first[c(FALSE, TRUE), c(FALSE, TRUE)], 2)

  expect_equal(pm_first[TRUE, TRUE], pm_first)

  expect_equal(m_first[c(TRUE, FALSE, FALSE), c(TRUE, FALSE, FALSE)], 3)
  expect_equal(m_first[c(FALSE, TRUE, FALSE), c(TRUE, FALSE, FALSE)], 7)
  expect_equal(m_first[c(TRUE, FALSE, FALSE), c(FALSE, TRUE, FALSE)], 0)
  expect_equal(m_first[c(FALSE, TRUE, FALSE), c(FALSE, TRUE, FALSE)], 1)

  expect_equal(m_first[c(TRUE, FALSE), c(TRUE, FALSE)],
               matrix(c(3, 2, 4, 8), 2, 2, byrow = TRUE))
  expect_equal(m_first[c(FALSE, TRUE), c(TRUE, FALSE)], c(7, 0))
  expect_equal(m_first[c(TRUE, FALSE), c(FALSE, TRUE)], c(0, 2))
  expect_equal(m_first[c(FALSE, TRUE), c(FALSE, TRUE)], 1)

  expect_equal(m_first[TRUE, TRUE], m_first)
})

test_that("row access", {
  init_data()

  expect_equal(pm_first[1,],
               polyMatrix(matrix(c(1, 1, 0, 1, 0, 3, 2, 4, 0), 1, 9, byrow = TRUE),
                          1, 3, 2))
  expect_equal(pm_first[c(3, 1),],
               polyMatrix(matrix(c(3, 0, 1, 0, 0, 0, 0, 6, 0,
                                   1, 1, 0, 1, 0, 3, 2, 4, 0), 2, 9, byrow = TRUE),
                          2, 3, 2))
  expect_equal(pm_first[c(TRUE, FALSE),],
               polyMatrix(matrix(c(1, 1, 0, 1, 0, 3, 2, 4, 0,
                                   3, 0, 1, 0, 0, 0, 0, 6, 0), 2, 9, byrow = TRUE),
                          2, 3, 2))
  expect_equal(pm_first[c(TRUE, FALSE, FALSE),],
               polyMatrix(matrix(c(1, 1, 0, 1, 0, 3, 2, 4, 0), 1, 9, byrow = TRUE), 1, 3, 2))
  expect_equal(pm_first[c(FALSE, TRUE),],
               polyMatrix(matrix(c(0, 2, 0, 3, 0, 0, 1, 0, 0), 1, 9, byrow = TRUE), 1, 3, 2))

  expect_equal(m_first[1,], c(3, 0, 2))
  expect_equal(m_first[c(3, 1),], matrix(c(4, 2, 8, 3, 0, 2), 2, 3, byrow = TRUE))
  expect_equal(m_first[c(TRUE, FALSE),], matrix(c(3, 0, 2, 4, 2, 8), 2, 3, byrow = TRUE))
  expect_equal(m_first[c(TRUE, FALSE, FALSE),], c(3, 0, 2))
  expect_equal(m_first[c(FALSE, TRUE),], c(7, 1, 0))
})

test_that("columns access", {
  init_data()

  expect_equal(pm_first[, 1],
               polyMatrix(matrix(c(1, 1, 2,
                                   0, 3, 1,
                                   3, 0, 0), 3, 3, byrow = TRUE),
                          3, 1, 2))
  expect_equal(pm_first[, c(3, 1)],
               polyMatrix(matrix(c(0, 1, 3, 1, 0, 2,
                                   0, 0, 0, 3, 0, 1,
                                   1, 3, 0, 0, 0, 0), 3, 6, byrow = TRUE),
                          3, 2, 2))
  expect_equal(pm_first[, c(TRUE, FALSE)],
               polyMatrix(matrix(c(1, 0, 1, 3, 2, 0,
                                   0, 0, 3, 0, 1, 0,
                                   3, 1, 0, 0, 0, 0), 3, 6, byrow = TRUE),
                          3, 2, 2))
  expect_equal(pm_first[, c(TRUE, FALSE, FALSE)],
               polyMatrix(matrix(c(1, 1, 2,
                                   0, 3, 1,
                                   3, 0, 0), 3, 3, byrow = TRUE),
                          3, 1, 2))
  expect_equal(pm_first[, c(FALSE, TRUE)],
               polyMatrix(matrix(c(1, 0, 4,
                                   2, 0, 0,
                                   0, 0, 6), 3, 3, byrow = TRUE),
                          3, 1, 2))

  expect_equal(m_first[, 1], c(3, 7, 4))
  expect_equal(m_first[, c(3, 1)], matrix(c(2, 3, 0, 7, 8, 4), 3, 2, byrow = TRUE))
  expect_equal(m_first[, c(TRUE, FALSE)], matrix(c(3, 2, 7, 0, 4, 8), 3, 2, byrow = TRUE))
  expect_equal(m_first[, c(TRUE, FALSE, FALSE)], c(3, 7, 4))
  expect_equal(m_first[, c(FALSE, TRUE)], c(0, 1, 2))
})

test_that("set one item: numeric and matrix", {
  init_data()

  pm_first[1, 1] <- 9
  expect_equal(pm_first,
               polyMatrix(matrix(c(9, 1, 0, 0, 0, 3, 0, 4, 0,
                                   0, 2, 0, 3, 0, 0, 1, 0, 0,
                                   3, 0, 1, 0, 0, 0, 0, 6, 0), 3, 9, byrow = TRUE),
                          3, 3, 2))
  expect_error(pm_first[1, 1] <- c(1, 2))
  expect_error(pm_first[1, 1] <- c())
  pm_first[2, 1] <- matrix(8, 1, 1)
  expect_equal(pm_first,
               polyMatrix(matrix(c(9, 1, 0, 0, 0, 3, 0, 4, 0,
                                   8, 2, 0, 0, 0, 0, 0, 0, 0,
                                   3, 0, 1, 0, 0, 0, 0, 6, 0), 3, 9, byrow = TRUE),
                          3, 3, 2))
  expect_error(pm_first[1, 1] <- matrix(1, 2, 2))
  pm_first[2, 3] <- 5
  expect_equal(pm_first,
               polyMatrix(matrix(c(9, 1, 0, 0, 0, 3, 0, 4, 0,
                                   8, 2, 5, 0, 0, 0, 0, 0, 0,
                                   3, 0, 1, 0, 0, 0, 0, 6, 0), 3, 9, byrow = TRUE),
                          3, 3, 2))
  pm_second[2, 3] <- 0
  expect_equal(pm_second, polyMatrix(0, 3, 3, 0))

  m_first[1, 2] <- 1
  expect_equal(m_first, matrix(c(3, 1, 2,
                                 7, 1, 0,
                                 4, 2, 8), 3, 3, byrow = TRUE))
  expect_error(m_first[1, 1] <- c(1, 2))
  expect_error(m_first[1, 1] <- c())
  m_first[1, 1] <- matrix(1, 1, 1)
  expect_equal(m_first, matrix(c(1, 1, 2,
                                 7, 1, 0,
                                 4, 2, 8), 3, 3, byrow = TRUE))
  expect_error(m_first[1, 1] <- matrix(1, 2, 2))
})

test_that("set one item: polynomail and polyMatrix", {
  init_data()

  pm_first[1, 1] <- p(7, 6)
  expect_equal(pm_first,
               polyMatrix(matrix(c(7, 1, 0, 6, 0, 3, 0, 4, 0,
                                   0, 2, 0, 3, 0, 0, 1, 0, 0,
                                   3, 0, 1, 0, 0, 0, 0, 6, 0), 3, 9, byrow = TRUE),
                          3, 3, 2))

  pm_second[2, 3] <- p(3, 4, 5)
  expect_equal(pm_second,
               polyMatrix(matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0,
                                   0, 0, 3, 0, 0, 4, 0, 0, 5,
                                   0, 0, 0, 0, 0, 0, 0, 0, 0), 3, 9, byrow = TRUE),
                          3, 3, 2))

  pm_second[2, 3] <- p(3, 4)
  expect_equal(pm_second,
               polyMatrix(matrix(c(0, 0, 0, 0, 0, 0,
                                   0, 0, 3, 0, 0, 4,
                                   0, 0, 0, 0, 0, 0), 3, 6, byrow = TRUE),
                          3, 3, 1))

  pm_second[1, 2] <- p(3, 2, 1)
  expect_equal(pm_second,
               polyMatrix(matrix(c(0, 3, 0, 0, 2, 0, 0, 1, 0,
                                   0, 0, 3, 0, 0, 4, 0, 0, 0,
                                   0, 0, 0, 0, 0, 0, 0, 0, 0), 3, 9, byrow = TRUE),
                          3, 3, 2))

  pm_first[2, 3] <- polyMatrix(matrix(c(2, 4, 6), 1, 3), 1, 1, 2)
  expect_equal(pm_first,
               polyMatrix(matrix(c(7, 1, 0, 6, 0, 3, 0, 4, 0,
                                   0, 2, 2, 3, 0, 4, 1, 0, 6,
                                   3, 0, 1, 0, 0, 0, 0, 6, 0), 3, 9, byrow = TRUE),
                          3, 3, 2))
})

test_that("set 2x2 items: number and matrix", {
  init_data()

  pm_first[c(2, 1), c(3, 1)] <- 9
  expect_equal(pm_first,
               polyMatrix(matrix(c(9, 1, 9, 0, 0, 0, 0, 4, 0,
                                   9, 2, 9, 0, 0, 0, 0, 0, 0,
                                   3, 0, 1, 0, 0, 0, 0, 6, 0), 3, 9, byrow = TRUE),
                          3, 3, 2))
  pm_second[c(2, 1), c(3, 1)] <- 0
  expect_equal(pm_second, polyMatrix(0, 3, 3, 0))

  m_first[c(2, 1), c(3, 1)] <- 9
  expect_equal(m_first, matrix(c(9, 0, 9, 9, 1, 9, 4, 2, 8), 3, 3, byrow = TRUE))
})

test_that("set 2x2: matrix", {
  init_data()

  pm_first[c(2, 1), c(3, 1)] <- matrix(c(2, 4, 6, 8), 2, 2, byrow = TRUE)
  expect_equal(pm_first,
               polyMatrix(matrix(c(8, 1, 6, 0, 0, 0, 0, 4, 0,
                                   4, 2, 2, 0, 0, 0, 0, 0, 0,
                                   3, 0, 1, 0, 0, 0, 0, 6, 0), 3, 9, byrow = TRUE),
                          3, 3, 2))
  pm_second[c(2, 1), c(3, 1)] <- matrix(c(0, 3, 6, 9), 2, 2, byrow = TRUE)
  expect_equal(pm_second,
               polyMatrix(matrix(c(9, 0, 6, 3, 0, 0, 0, 0, 0), 3, 3, byrow = TRUE), 3, 3, 0))


  m_first[c(3, 2), c(2, 1)] <- matrix(c(2, 4, 6, 8), 2, 2, byrow = TRUE)
  expect_equal(m_first, matrix(c(3, 0, 2, 8, 6, 0, 4, 2, 8), 3, 3, byrow = TRUE))
})

test_that("set 2x2: polynomail", {
  init_data()

  pm_first[c(2, 1), c(3, 1)] <- p(7, 2)
  expect_equal(pm_first,
               polyMatrix(matrix(c(7, 1, 7, 2, 0, 2, 0, 4, 0,
                                   7, 2, 7, 2, 0, 2, 0, 0, 0,
                                   3, 0, 1, 0, 0, 0, 0, 6, 0), 3, 9, byrow = TRUE),
                          3, 3, 2))

  pm_second[c(2, 1), c(3, 1)] <- p(2, 4)
  expect_equal(pm_second,
               polyMatrix(matrix(c(2, 0, 2, 4, 0, 4,
                                   2, 0, 2, 4, 0, 4,
                                   0, 0, 0, 0, 0, 0), 3, 6, byrow = TRUE),
                          3, 3, 1))
  pm_second[c(3, 1), c(1, 2)] <- p(3, 5, 7)
  expect_equal(pm_second,
               polyMatrix(matrix(c(3, 3, 2, 5, 5, 4, 7, 7, 0,
                                   2, 0, 2, 4, 0, 4, 0, 0, 0,
                                   3, 3, 0, 5, 5, 0, 7, 7, 0), 3, 9, byrow = TRUE),
                          3, 3, 2))
})

test_that("set 2x2: polyMatrix", {
  pm_second[c(2, 1), c(3, 1)] <- polyMatrix(matrix(c(1, 3, 2, 4,
                                                     5, 7, 6, 8), 2, 4, byrow = TRUE),
                                            2, 2, 1)
  expect_equal(pm_second,
               polyMatrix(matrix(c(7, 0, 5, 8, 0, 6,
                                   3, 0, 1, 4, 0, 2,
                                   0, 0, 0, 0, 0, 0), 3, 6, byrow = TRUE),
                          3, 3, 1))

  pm_second[c(2, 1), c(3, 1)] <- polyMatrix(matrix(c(1, 2, 3, 4, 1, 1,
                                                     5, 6, 7, 8, 2, 2), 2, 6, byrow = TRUE),
                                            2, 2, 2)
  expect_equal(pm_second,
               polyMatrix(matrix(c(6, 0, 5, 8, 0, 7, 2, 0, 2,
                                   2, 0, 1, 4, 0, 3, 1, 0, 1,
                                   0, 0, 0, 0, 0, 0, 0, 0, 0), 3, 9, byrow = TRUE),
                          3, 3, 2))
})

test_that("set one row", {
  init_data()

  pm_first[1,] <- 1
  expect_equal(pm_first,
               polyMatrix(matrix(c(1, 1, 1, 0, 0, 0, 0, 0, 0,
                                   0, 2, 0, 3, 0, 0, 1, 0, 0,
                                   3, 0, 1, 0, 0, 0, 0, 6, 0), 3, 9, byrow = TRUE),
                          3, 3, 2))


  pm_first[1,] <- matrix(c(3, 4, 5), 1, 3)
  expect_equal(pm_first,
               polyMatrix(matrix(c(3, 4, 5, 0, 0, 0, 0, 0, 0,
                                   0, 2, 0, 3, 0, 0, 1, 0, 0,
                                   3, 0, 1, 0, 0, 0, 0, 6, 0), 3, 9, byrow = TRUE),
                          3, 3, 2))
  pm_first[1,] <- p(2, 3)

  expect_equal(pm_first,
               polyMatrix(matrix(c(2, 2, 2, 3, 3, 3, 0, 0, 0,
                                   0, 2, 0, 3, 0, 0, 1, 0, 0,
                                   3, 0, 1, 0, 0, 0, 0, 6, 0), 3, 9, byrow = TRUE),
                          3, 3, 2))

  pm_first[1,] <- polyMatrix(matrix(c(1, 2, 0, 3, 0, 0), 1, 6, byrow = TRUE), 1, 3, 1)
  expect_equal(pm_first,
               polyMatrix(matrix(c(1, 2, 0, 3, 0, 0, 0, 0, 0,
                                   0, 2, 0, 3, 0, 0, 1, 0, 0,
                                   3, 0, 1, 0, 0, 0, 0, 6, 0), 3, 9, byrow = TRUE),
                          3, 3, 2))
})

test_that("set two rows", {
  init_data()

  pm_first[c(3, 1),] <- 1
  expect_equal(pm_first,
               polyMatrix(matrix(c(1, 1, 1, 0, 0, 0, 0, 0, 0,
                                   0, 2, 0, 3, 0, 0, 1, 0, 0,
                                   1, 1, 1, 0, 0, 0, 0, 0, 0), 3, 9, byrow = TRUE),
                          3, 3, 2))


  pm_first[c(3, 1),] <- matrix(c(3, 4, 5, 1, 2, 3), 2, 3, byrow = TRUE)
  expect_equal(pm_first,
               polyMatrix(matrix(c(1, 2, 3, 0, 0, 0, 0, 0, 0,
                                   0, 2, 0, 3, 0, 0, 1, 0, 0,
                                   3, 4, 5, 0, 0, 0, 0, 0, 0), 3, 9, byrow = TRUE),
                          3, 3, 2))
  pm_first[c(3, 1),] <- p(2, 3)

  expect_equal(pm_first,
               polyMatrix(matrix(c(2, 2, 2, 3, 3, 3, 0, 0, 0,
                                   0, 2, 0, 3, 0, 0, 1, 0, 0,
                                   2, 2, 2, 3, 3, 3, 0, 0, 0), 3, 9, byrow = TRUE),
                          3, 3, 2))

  pm_first[c(3, 1),] <- polyMatrix(matrix(c(1, 2, 0, 3, 0, 0,
                                            0, 9, 8, 0, 0, 7), 2, 6, byrow = TRUE), 2, 3, 1)
  expect_equal(pm_first,
               polyMatrix(matrix(c(0, 9, 8, 0, 0, 7, 0, 0, 0,
                                   0, 2, 0, 3, 0, 0, 1, 0, 0,
                                   1, 2, 0, 3, 0, 0, 0, 0, 0), 3, 9, byrow = TRUE),
                          3, 3, 2))
})

test_that("set one column", {
  init_data()

  pm_first[, 1] <- 2
  expect_equal(pm_first,
               polyMatrix(matrix(c(2, 1, 0, 0, 0, 3, 0, 4, 0,
                                   2, 2, 0, 0, 0, 0, 0, 0, 0,
                                   2, 0, 1, 0, 0, 0, 0, 6, 0), 3, 9, byrow = TRUE),
                          3, 3, 2))
  pm_first[, 1] <- matrix(c(3, 2, 1), 3, 1, byrow = TRUE)
  expect_equal(pm_first,
               polyMatrix(matrix(c(3, 1, 0, 0, 0, 3, 0, 4, 0,
                                   2, 2, 0, 0, 0, 0, 0, 0, 0,
                                   1, 0, 1, 0, 0, 0, 0, 6, 0), 3, 9, byrow = TRUE),
                          3, 3, 2))
  pm_first[, 1] <- p(1, 9)
  expect_equal(pm_first,
               polyMatrix(matrix(c(1, 1, 0, 9, 0, 3, 0, 4, 0,
                                   1, 2, 0, 9, 0, 0, 0, 0, 0,
                                   1, 0, 1, 9, 0, 0, 0, 6, 0), 3, 9, byrow = TRUE),
                          3, 3, 2))
  pm_first[, 1] <- polyMatrix(matrix(c(2, 5, 0, 8, 7, 0), 3, 2, byrow = TRUE), 3, 1, 1)
  expect_equal(pm_first,
               polyMatrix(matrix(c(2, 1, 0, 5, 0, 3, 0, 4, 0,
                                   0, 2, 0, 8, 0, 0, 0, 0, 0,
                                   7, 0, 1, 0, 0, 0, 0, 6, 0), 3, 9, byrow = TRUE),
                          3, 3, 2))
})

test_that("set tow columns", {
  init_data()

  pm_first[, c(3, 1)] <- 2
  expect_equal(pm_first,
               polyMatrix(matrix(c(2, 1, 2, 0, 0, 0, 0, 4, 0,
                                   2, 2, 2, 0, 0, 0, 0, 0, 0,
                                   2, 0, 2, 0, 0, 0, 0, 6, 0), 3, 9, byrow = TRUE),
                          3, 3, 2))
  pm_first[, c(3, 1)] <- matrix(c(9, 3, 8, 2, 7, 1), 3, 2, byrow = TRUE)
  expect_equal(pm_first,
               polyMatrix(matrix(c(3, 1, 9, 0, 0, 0, 0, 4, 0,
                                   2, 2, 8, 0, 0, 0, 0, 0, 0,
                                   1, 0, 7, 0, 0, 0, 0, 6, 0), 3, 9, byrow = TRUE),
                          3, 3, 2))
  pm_first[, c(3, 1)] <- p(1, 9)
  expect_equal(pm_first,
               polyMatrix(matrix(c(1, 1, 1, 9, 0, 9, 0, 4, 0,
                                   1, 2, 1, 9, 0, 9, 0, 0, 0,
                                   1, 0, 1, 9, 0, 9, 0, 6, 0), 3, 9, byrow = TRUE),
                          3, 3, 2))
  pm_first[, c(3, 1)] <- polyMatrix(matrix(c(1, 2, 4, 5,
                                             0, 0, 5, 8,
                                             2, 7, 0, 0), 3, 4, byrow = TRUE),
                                    3, 2, 1)
  expect_equal(pm_first,
               polyMatrix(matrix(c(2, 1, 1, 5, 0, 4, 0, 4, 0,
                                   0, 2, 0, 8, 0, 5, 0, 0, 0,
                                   7, 0, 2, 0, 0, 0, 0, 6, 0), 3, 9, byrow = TRUE),
                          3, 3, 2))
})

test_that("set whole matrix", {
  init_data()

  pm_small[,] <- 6
  expect_equal(pm_small,
               polyMatrix(matrix(c(6, 6, 6, 6), 2, 2, byrow = TRUE), 2, 2, 0))
  pm_small[,] <- p(2, 8)
  expect_equal(pm_small,
               polyMatrix(matrix(c(2, 2, 8, 8, 2, 2, 8, 8), 2, 4, byrow = TRUE), 2, 2, 1))
})

################################################################################################
# custom text from real bugs

test_that("custom 1", {
  pm <- parse.polyMatrix("1, x + 2",
                         "0, x^2",
                         "0, 0")
  expect_equal(pm[, 1], parse.polyMatrix("1", "0", "0"))
  expect_equal(pm[, 2], parse.polyMatrix("2 + x", "x^2", "0"))
})
