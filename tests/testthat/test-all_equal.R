test_that("all_equal works correctly for various types", {
  expect_true(all_equal(c(1, 1, 1, 1)))
  expect_true(all_equal(c("a", "a", "a")))
  expect_true(all_equal(c(TRUE, TRUE, TRUE)))
  expect_false(all_equal(c(1, 2, 1)))
  expect_false(all_equal(c("a", "b", "a")))
  expect_false(all_equal(c(TRUE, FALSE, TRUE)))
})

test_that("all_equal handles edge cases", {
  expect_true(all_equal(numeric(0)))  # Empty vector
  expect_true(all_equal(5))  # Single element
  expect_true(all_equal(NA))  # Single NA
  expect_true(all_equal(c(NA, NA, NA)))  # Multiple NAs
  expect_false(all_equal(c(NA, 1, NA)))  # Mixed NA and non-NA
})

test_that("all_equal works with different data types", {
  expect_true(all_equal(c(1L, 1L, 1L)))  # Integer
  expect_true(all_equal(c(1.0, 1.0, 1.0)))  # Double
  expect_true(all_equal(as.complex(c(1+0i, 1+0i, 1+0i))))  # Complex
  expect_true(all_equal(factor(c("a", "a", "a"))))  # Factor
})

test_that("all_equal correctly handles vectors of only NAs", {
  expect_true(all_equal(c(NA, NA, NA)))
  expect_true(all_equal(as.numeric(c(NA, NA, NA))))
  expect_true(all_equal(as.character(c(NA, NA, NA))))
  expect_true(all_equal(as.logical(c(NA, NA, NA))))
})
