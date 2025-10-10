
test_that("CumSum works for vector", {
  expect_equal(CumSum(1:4), c(0.1, 0.3, 0.6, 1))
  v <- c(1, NA, 2)
  expect_equal(CumSum(v), c(1/3, NA, 1))
})

test_that("CumSum works for data frame with groups", {
  df <- data.frame(val = c(1,2,3,4), group = c("A","A","B","B"))
  res <- CumSum(df, cum_var="val", group_vars="group", scale=FALSE)
  expect_equal(res, c(1,3,3,7))
})

test_that("CumSum handles order_by", {
  df <- data.frame(val = c(3,1,2), key = c(2,1,3))
  expect_equal(CumSum(df, cum_var="val", order_by="key"), c(4/6,1/6,6/6))
})

test_that("CumSum errors for invalid input", {
  df <- data.frame(a = 1:3)
  expect_error(CumSum(df, cum_var="notfound"), "cum_var does not exist in data")
  expect_error(CumSum(df, order_by="notfound", cum_var="a"), "order_by does not exist in data")
})

test_that("CumSum works for NA with na_rm=FALSE", {
  expect_equal(CumSum(c(1,NA,2), na_rm=FALSE), c(1, NA, NA))
})
