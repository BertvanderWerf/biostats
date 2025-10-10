# library(testthat)
#
# # Load the function to be tested
# source("../R/your_function_file.R")  # Adjust the path as necessary

# Sample data for tests
sample_names <- c("abc", "def", "ab", "bac", "abcd", "aq", "dqp")

test_that("check_names works correctly with valid inputs", {

  # Test basic functionality
  expect_equal(check_names(c(A="ab", "def"), sample_names, "test"),
               c(A="ab", def="def"))

  # Test partial matching
  expect_equal(check_names(c("ab", "de"), sample_names, "test"),
               c(ab="ab", def="def"))

  # Test case-insensitive matching
  expect_equal(check_names(c("AB", "DEF"), sample_names, "test", ignore_case=TRUE),
               c(ab="ab", def="def"))

  # Test return_index
  expect_equal(check_names(c("ab", "def"), sample_names, "test", return_index=TRUE),
               c(ab=3, def=2))

  # Test with named input
  expect_equal(check_names(c(X="ab", Y="def"), sample_names, "test"),
               c(X="ab", Y="def"))
})

test_that("check_names handles edge cases correctly", {

  # Test with NULL input
  expect_null(check_names(NULL, sample_names, "test"))

  # Test with single match when partial='Exact'
  expect_equal(check_names("abc", sample_names, "test", partial_match='Exact'),
               c(abc="abc"))

  # Test with exact match when partial='Start'
  expect_equal(check_names("abc", sample_names, "test", partial_match='Start'),
               c(abc="abc"))

  # Test with exact match when partial='Anywhere'
  expect_equal(check_names("bcd", sample_names, "test", partial_match='Anywhere'),
               c(abcd="abcd"))

})

test_that("check_names throws appropriate errors", {

  # Test with empty input
  expect_error(check_names(character(0), sample_names, "test"),
               "test: must be NULL or consist of character strings")

  # Test with NA in input
  expect_error(check_names(c("ab", NA), sample_names, "test"),
               "test: can't have empty or NA strings")

  # Test with empty string in input
  expect_error(check_names(c("ab", ""), sample_names, "test"),
               "test: can't have empty or NA strings")

  # Test with no matches
  expect_error(check_names("xyz", sample_names, "test"),
               "test: 'xyz' not found")

  # Test with multiple matches
  expect_error(check_names("a", sample_names, "test"),
               "test: The following inputs match multiple variables:")
})

test_that("check_names handles partial matching correctly", {

  # Test partial matching enabled
  expect_equal(check_names("ab", sample_names, "test", partial_match='Start'),
               c(ab="ab"))

  # Test partial matching disabled
  expect_error(check_names("ba", sample_names, "test", partial_match='Exact'),
               "test: 'ba' not found")
})

test_that("check_names handles case sensitivity correctly", {

  # Test case-sensitive matching
  expect_error(check_names("ABC", sample_names, "test", ignore_case=FALSE),
               "test: 'ABC' not found")

  # Test case-insensitive matching
  expect_equal(check_names("ABC", sample_names, "test", ignore_case=TRUE),
               c(abc="abc"))
})

test_that("check_names handles naming of results correctly", {

  # Test with named input
  expect_equal(names(check_names(c(X="ab", Y="def"), sample_names, "test")),
               c("X", "Y"))

  # Test with partially named input
  expect_equal(names(check_names(c(X="ab", "def"), sample_names, "test")),
               c("X", "def"))
})
