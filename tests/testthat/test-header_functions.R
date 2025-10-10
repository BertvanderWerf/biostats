# library(testthat)
#
# # Load the functions to be tested
# source("../R/header_functions.R")  # Adjust the path as necessary

# Test is.single_char function
test_that("is.single_char works correctly", {

  # Test valid single character input
  expect_true(is.single_char("a"))

  # Test invalid inputs
  expect_false(is.single_char("ab"))    # More than one character
  expect_false(is.single_char(""))       # Empty string
  expect_false(is.single_char(1))        # Numeric input
  expect_false(is.single_char(NULL))     # NULL input
  expect_false(is.single_char(c("a", "b")))  # Vector of characters

  # Test special characters and whitespace
  expect_true(is.single_char("@"))       # Special character
  expect_true(is.single_char(" "))       # Whitespace (single space)
})


# Test header_to_column_names function
test_that("header_to_column_names works correctly", {

  # Valid input
  header <- data.frame(A = c("a1"), B = c("b1"))
  concatenated <- header_to_column_names(header)
  expect_equal(concatenated, "A.a1:B.b1")

  # invalid sep_col_value
  expect_error(header_to_column_names(header, sep_col_value=":."),
               "sep_col_value must be a single character.")

  # invalid sep_col_value_pair
  expect_error(header_to_column_names(header, sep_col_value_pair=":."),
               "sep_col_value_pair must be a single character.")

  # Multiple rows
  header <- data.frame(A = c("a1", "a2"), B = c("b1", "b2"))
  concatenated <- header_to_column_names(header)
  expect_equal(concatenated, c("A.a1:B.b1", "A.a2:B.b2"))

  # Edge case: empty data frame
  header_empty <- data.frame()
  expect_error(header_to_column_names(header_empty),
               "Header must be a data.frame with at least one column and one value.")

  # Edge case: header <- NULL
  header_empty <- NULL
  expect_error(header_to_column_names(header_empty),
               "Header must be a data.frame with at least one column and one value.")

  # Edge case: header is not a data.frame
  header <- as.matrix(data.frame(A = c("a1", "a2"), B = c("b1", "b2")))
  expect_error(header_to_column_names(header_empty),
               "Header must be a data.frame with at least one column and one value.")

  # Edge case: single column with no values
  header_single_empty <- data.frame(A = character(0))
  expect_error(header_to_column_names(header_single_empty),
               "Header must be a data.frame with at least one column and one value.")

  # Edge case: invalid characters in values
  header_invalid <- data.frame(A = c("a1", "a:2"), B = c("b1", "b2"))
  expect_error(header_to_column_names(header_invalid),
               "Header values must not contain the specified separators.")
})

# Test column_names_to_header function
test_that("column_names_to_header works correctly", {

  # Valid input
  concatenated <- "A.a1:B.b1"
  header_reconstructed <- column_names_to_header(concatenated)
  expect_equal(header_reconstructed$A, c("a1"))
  expect_equal(header_reconstructed$B, c("b1"))

  # invalid sep_col_value
  expect_error(column_names_to_header(concatenated, sep_col_value=":."),
               "sep_col_value must be a single character.")

  # invalid sep_col_value_pair
  expect_error(column_names_to_header(concatenated, sep_col_value_pair=":."),
               "sep_col_value_pair must be a single character.")

  # Multiple rows
  concatenated <- c("A.a1:B.b1", "A.a2:B.b2")
  header_reconstructed <- column_names_to_header(concatenated)
  expect_equal(header_reconstructed, data.frame(A = c("a1", "a2"), B = c("b1", "b2")))

  # Edge case: empty pair
  concatenated <- c("A.a1:B.b1", "A.a2:B.b2", ":.")
  expect_error(column_names_to_header(concatenated),
               "Input must contain valid column name-value pairs.")

  # Edge case: empty string input
  expect_error(column_names_to_header(""),
               "Input must contain strings with valid column name-value pairs.")

  # Edge case: NULL input
  expect_error(column_names_to_header(NULL),
               "Input must contain strings with valid column name-value pairs.")


  # Edge case: invalid format
  expect_error(column_names_to_header("A.a1:B.b1:A"),
               "Input must contain valid column name-value pairs.")

  # Edge case: invalid format
  expect_error(column_names_to_header(c("A.a1:B.b1:A", "A.a2:B.b2")),
               "Each input item should contain equal number of column-value pairs.")
})

