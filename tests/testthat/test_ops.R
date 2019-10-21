context("Ops")

test_that("equals of a-matrix", {
  t <- polyMgen.a(2, 3, c(1, 2, 3, 1, 1), degree=2)
  a <- polyMgen.a(3, 3, c(1, 2, 2, 1, 1), degree=2)
  b <- polyMgen.a(2, 2, c(1, 2, 2, 1, 1), degree=2)
  c <- polyMgen.a(2, 3, c(2, 2, 2, 1, 1), degree=2)
  d <- polyMgen.a(2, 3, c(1, 2, 2, 1, 1), degree=1)
  expect_true(t == t)
  expect_false(t == a)
  expect_false(t == b)
  expect_false(t == c)
  expect_false(t == d)

  expect_true(t %==% t)
  expect_false(t %==% a)
  expect_false(t %==% b)
  expect_false(t %==% c)
  expect_false(t %==% d)

  expect_false(t != t)
  expect_true(t != a)
  expect_true(t != b)
  expect_true(t != c)
  expect_true(t != d)

  expect_false(t %!=% t)
  expect_true(t %!=% a)
  expect_true(t %!=% b)
  expect_true(t %!=% c)
  expect_true(t %!=% d)
})

test_that("equals of d-matrix", {
  t <- polyMgen.d(2, 3, list(1, 2, 3, 1, 1))
  a <- polyMgen.d(3, 3, list(1, 2, 2, 1, 1))
  b <- polyMgen.d(2, 2, list(1, 2, 2, 1, 1))
  c <- polyMgen.d(2, 3, list(2, 2, 2, 1, 1))
  d <- polyMgen.d(2, 3, list(1, 2, 2, 1, 1))
  expect_true(t == t)
  expect_false(t == a)
  expect_false(t == b)
  expect_false(t == c)
  expect_false(t == d)

  expect_true(t %==% t)
  expect_false(t %==% a)
  expect_false(t %==% b)
  expect_false(t %==% c)
  expect_false(t %==% d)

  expect_false(t != t)
  expect_true(t != a)
  expect_true(t != b)
  expect_true(t != c)
  expect_true(t != d)

  expect_false(t %!=% t)
  expect_true(t %!=% a)
  expect_true(t %!=% b)
  expect_true(t %!=% c)
  expect_true(t %!=% d)
})

p <- polynom::polynomial

test_that("unary operators", {
  a <- polyMgen.d(2, 3, list(1, p(c(2, 3)), 3, 1, p(c(1, 3))))
  ma <- polyMgen.d(2, 3, list(-1, p(c(-2, -3)), -3, -1, p(c(-1, -3))))
  expect_true(a == +a)
  expect_true(-a == ma)
})

test_that("sclalar operators", {
  a <- polyMgen.d(2, 3, list(1, p(c(2, 3)), 3, 1, p(c(1, 3))))

  expect_true(a + 0 == a)
  expect_true(0 + a == a)
  expect_true(a %+% 0 == a)
  expect_true(0 %+% a == a)

  aP1 <- polyMgen.d(2, 3, list(1 + 1, p(c(2 + 1, 3)), 3 + 1, 1 + 1, p(c(1 + 1, 3))))
  expect_true(a + 1 == aP1)
  expect_true(1 + a == aP1)
  expect_true(a %+% 1 == aP1)
  expect_true(1 %+% a == aP1)

  aP1x1 = polyMgen.d(2, 3, list(p(c(1 + 1, 1)), p(c(2 + 1, 3 + 1)), p(c(3 + 1, 1)), p(c(1 + 1, 1)), p(c(1 + 1, 3 + 1))))
  expect_true(a %+% p(c(1, 1)) == aP1x1)
  expect_true(p(c(1, 1)) %+% a == aP1x1)

  expect_true(a - 0 == a)
  expect_true(0 - a == -a)
  expect_true(a %-% 0 == a)
  expect_true(0 %-% a == -a)

  aM1 <- polyMgen.d(2, 3, list(1 - 1, p(c(2 - 1, 3)), 3 - 1, 1 - 1, p(c(1 - 1, 3))))
  expect_true(a %-% 1 == aM1)
  expect_true(1 %-% a == -aM1)

  aM1x1 = polyMgen.d(2, 3, list(p(c(1 - 1, -1)), p(c(2 - 1, 3 - 1)), p(c(3 - 1, -1)), p(c(1 - 1, -1)), p(c(1 - 1, 3 - 1))))
  expect_true(a %-% p(c(1, 1)) == aM1x1)
  expect_true(p(c(1, 1)) %-% a == -aM1x1)

  expect_true(a * 1 == a)
  expect_true(1 * a == a)
  expect_true(a %X% 1 == a)
  expect_true(1 %X% a == a)

  aMul2 <- polyMgen.d(2, 3, list(2 * 1, 2 * p(c(2, 3)), 2 * 3, 2 * 1, 2 * p(c(1, 3))))
  expect_true(a * 2 == aMul2)
  expect_true(2 * a == aMul2)
  expect_true(a %X% 2 == aMul2)
  expect_true(2 %X% a == aMul2)

  aMul1x1 <- polyMgen.d(2, 3, list(p(c(1, 1)), p(c(2, 5, 3)), p(c(3, 3)), p(c(1, 1)), p(c(1, 4,3))))
  expect_true(a %X% p(c(1, 1)) == aMul1x1)
  expect_true(p(c(1, 1)) %X% a == aMul1x1)
})

test_that("matrix operators", {
  a <-   polyMgen.d(2, 3, list(1,           p(c(2, 3)), 3,            1, p(c(3, 1))))
  b <-   polyMgen.d(2, 3, list(p(c(1, 3)),  2,          p(c(2, 3)),   3, p(c(1, 3))))
  aPb <- polyMgen.d(2, 3, list(p(c(2, 3)),  p(c(4, 3)), p(c(5, 3)),   4, p(c(4, 4))))
  aMb <- polyMgen.d(2, 3, list(p(c(0, -3)), p(c(0, 3)), p(c(1, -3)), -2, p(c(2, -2))))
  expect_true(a + b == aPb)
  expect_true(a - b == aMb)
  expect_true(b - a == -aMb)
  expect_true(a %+% b == aPb)
  expect_true(a %-% b == aMb)
  expect_true(b %-% a == -aMb)
})

test_that("matrix mult", {
  a <-   polyMgen.d(2, 3, list(1,           p(c(2, 3)), 3,            1, p(c(3, 1))))
  b <-   polyMgen.d(3, 2, list(p(c(1, 3)),  2,          p(c(2, 3)),   3, p(c(1, 3))))
  aMULb <- polyMgen.d(2, 2, list(p(c(13, 14, 3)), p(c(6, 12, 9)), p(c(9, 19, 3)), p(c(8, 15))))
  expect_true(a * b == aMULb)
  expect_true(a %X% b == aMULb)
})

test_that("matrix power", {
  a <- polyMgen.d(2, 2, list(1, 2, 3, p(c(1, 1))))
  expect_true(a ^ 0 == polyMgen.d(2, 2, list(1, 0, 0, 1)))
  expect_true(a ^ 1 == a)

  expect_true(a ^ 2 == polyMgen.d(2, 2, list(7, p(c(4, 2)), p(c(6, 3)), p(c(7, 2, 1)))))
  expect_true(a ^ 3 == a %X% a %X% a)
})
