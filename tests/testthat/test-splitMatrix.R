test_that("splitMatrix() splits a matrix", {
  m <- matrix(runif(100), 10, 10)
  sm <- splitMatrix(m, 2, 2)
  expect_equal(TRUE, all(sm[1,1][[1]]==m[1:5, 1:5]))
  expect_equal(TRUE, all(sm[1,2][[1]]==m[1:5, 6:10]))
  expect_equal(TRUE, all(sm[2,1][[1]]==m[6:10, 1:5]))
  expect_equal(TRUE, all(sm[2,2][[1]]==m[6:10, 6:10]))
})


