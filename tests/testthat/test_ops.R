context("Ops")

test_that("equals of a-matrix", {
  t <- pMgen.a(2, 3, c(1, 2, 3, 1, 1), degree=2)
  a <- pMgen.a(3, 3, c(1, 2, 2, 1, 1), degree=2)
  b <- pMgen.a(2, 2, c(1, 2, 2, 1, 1), degree=2)
  c <- pMgen.a(2, 3, c(2, 2, 2, 1, 1), degree=2)
  d <- pMgen.a(2, 3, c(1, 2, 2, 1, 1), degree=1)
  expect_true(t == t)
  expect_false(t == a)
  expect_false(t == b)
  expect_false(t == c)
  expect_false(t == d)

  expect_false(t != t)
  expect_true(t != a)
  expect_true(t != b)
  expect_true(t != c)
  expect_true(t != d)
})

test_that("equals of d-matrix", {
  t <- pMgen.d(2, 3, list(1, 2, 3, 1, 1))
  a <- pMgen.d(3, 3, list(1, 2, 2, 1, 1))
  b <- pMgen.d(2, 2, list(1, 2, 2, 1, 1))
  c <- pMgen.d(2, 3, list(2, 2, 2, 1, 1))
  d <- pMgen.d(2, 3, list(1, 2, 2, 1, 1))
  expect_true(t == t)
  expect_false(t == a)
  expect_false(t == b)
  expect_false(t == c)
  expect_false(t == d)

  expect_false(t != t)
  expect_true(t != a)
  expect_true(t != b)
  expect_true(t != c)
  expect_true(t != d)
})

p <- polynom::polynomial

test_that("unary operators", {
  a <- pMgen.d(2, 3, list(1, p(c(2, 3)), 3, 1, p(c(1, 3))))
  ma <- pMgen.d(2, 3, list(-1, p(c(-2, -3)), -3, -1, p(c(-1, -3))))
  expect_true(a == +a)
  expect_true(-a == ma)
})

test_that("sclalar operators", {
  a <- pMgen.d(2, 3, list(1, p(c(2, 3)), 3, 1, p(c(1, 3))))

  expect_true(a + 0 == a)
  expect_true(0 + a == a)
  aP1 <- pMgen.d(2, 3, list(1 + 1, p(c(2 + 1, 3)), 3 + 1, 1 + 1, p(c(1 + 1, 3))))
  expect_true(a + 1 == aP1)
  expect_true(1 + a == aP1)
  # aP1x1 = pMgen.d(2, 3, list(p(c(1 + 1, 1)), p(c(2 + 1, 3 + 1)), p(c(3 + 1, 1)), p(c(1 + 1, 1)), p(c(1 + 1, 3 + 1))))
  # expect_true(a + p(c(1, 1)) == aP1x1)
  # expect_true(p(c(1, 1)) + a == aP1x1)

  expect_true(a - 0 == a)
  expect_true(0 - a == -a)
  aM1 <- pMgen.d(2, 3, list(1 - 1, p(c(2 - 1, 3)), 3 - 1, 1 - 1, p(c(1 - 1, 3))))
  expect_true(a - 1 == aM1)
  expect_true(1 - a == -aM1)
  # aM1x1 = pMgen.d(2, 3, list(p(c(1 - 1, -1)), p(c(2 - 1, 3 - 1)), p(c(3 - 1, -1)), p(c(1 - 1, -1)), p(c(1 - 1, 3 - 1))))
  # expect_true(a - p(c(1, 1)) == aM1x1)
  # expect_true(p(c(1, 1)) - a == -aM1x1)

  expect_true(a * 1 == a)
  expect_true(1 * a == a)
  aMul2 <- pMgen.d(2, 3, list(2 * 1, 2 * p(c(2, 3)), 2 * 3, 2 * 1, 2 * p(c(1, 3))))
  expect_true(a * 2 == aMul2)
  expect_true(2 * a == aMul2)
})

test_that("matrix operators", {
  a <-   pMgen.d(2, 3, list(1,           p(c(2, 3)), 3,            1, p(c(3, 1))))
  b <-   pMgen.d(2, 3, list(p(c(1, 3)),  2,          p(c(2, 3)),   3, p(c(1, 3))))
  aPb <- pMgen.d(2, 3, list(p(c(2, 3)),  p(c(4, 3)), p(c(5, 3)),   4, p(c(4, 4))))
  aMb <- pMgen.d(2, 3, list(p(c(0, -3)), p(c(0, 3)), p(c(1, -3)), -2, p(c(2, -2))))
  expect_true(a + b == aPb)
  expect_true(a - b == aMb)
  expect_true(b - a == -aMb)
})

test_that("matrix mult", {
  a <-   pMgen.d(2, 3, list(1,           p(c(2, 3)), 3,            1, p(c(3, 1))))
  b <-   pMgen.d(3, 2, list(p(c(1, 3)),  2,          p(c(2, 3)),   3, p(c(1, 3))))
  aMULb <- pMgen.d(2, 2, list(p(c(13, 14, 3)), p(c(6, 12, 9)), p(c(9, 19, 3)), p(c(8, 15))))
  expect_true(a * b == aMULb)
})

test_that("matrix power", {
  a <- pMgen.d(2, 2, list(1, 2, 3, p(c(1, 1))))
  expect_true(a ^ 0 == pMgen.d(2, 2, list(1, 0, 0, 1)))
  expect_true(a ^ 1 == a)

  expect_true(a ^ 2 == pMgen.d(2, 2, list(7, p(c(4, 2)), p(c(6, 3)), p(c(7, 2, 1)))))
  expect_true(a ^ 3 == a * a * a)
})
