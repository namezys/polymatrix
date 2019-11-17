context("utils cyc fill")

test_that("pMdiag", {
  expect_true(
    pMdiag(1, 2) ==
    polyMgen(rawData=list(1, 0, 0, 1), degree=0, ncol=2, nrow=2)
  )
  expect_true(
    pMdiag(polynom::polynomial(2), 2) ==
      polyMgen(rawData=list(2, 0, 0, 2), degree=0, ncol=2, nrow=2)
  )
  expect_true(
    pMdiag(list(polynom::polynomial(2), polynom::polynomial(1)), 3) ==
      polyMgen(degree=0, ncol=3, nrow=3, byrow=TRUE, rawData=list(
        2, 0, 0,
        0, 1, 0,
        0, 0, 2
        ))
  )
})
