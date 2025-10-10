test_that("check_function_names works correctly with valid inputs", {

  # Test valid function names
  result <- check_function_names(c('base::mean', 'median', Sum='base::sum'), error_call_string = "example_function")
  expect_true(is.function(result$mean))
  expect_true(is.function(result$median))
  expect_true(is.function(result$Sum))

  # Test with NULL input and default function
  result <- check_function_names(NULL, error_call_string = "example_function")
  expect_true(is.function(result$count))  # Default function should be 'count'

  # Test with NULL input and a different default function
  result <- check_function_names(NULL, default = 'median', error_call_string = "example_function")
  expect_true(is.function(result$median))  # Default should be 'stats::median'
})

test_that("check_function_names throws errors for invalid inputs", {

  # Test empty character vector
  expect_error(check_function_names(c('', 'mean'), error_call_string = "example_function"),
               "example_function: cannot contain empty or NA strings", fixed = TRUE)

  # Test NA values in the input
  expect_error(check_function_names(c(NA, 'sum'), error_call_string = "example_function"),
               "example_function: cannot contain empty or NA strings", fixed = TRUE)

  # Test invalid function name
  expect_error(check_function_names(c('base::nonexistent_func'), error_call_string = "example_function"),
               "example_function: function 'base::nonexistent_func' not found", fixed = TRUE)

  # Test invalid input type (not character)
  expect_error(check_function_names(123, error_call_string = "example_function"),
               "example_function: must be NULL or a non-empty character vector", fixed = TRUE)

  # Test empty input (not NULL)
  expect_error(check_function_names(character(0), error_call_string = "example_function"),
               "example_function: must be NULL or a non-empty character vector", fixed = TRUE)
})

test_that("check_function_names handles unnamed functions correctly", {

  # Test unnamed functions
  funs <- c('base::mean', 'median')
  result <- check_function_names(funs, error_call_string = "example_function")
  expect_equal(names(result), c("mean", "median")) # Check if names are set correctly

})

test_that("check_function_names returns unique names for duplicate functions", {

  # Test duplicate functions
  funs <- c('base::sum', 'base::sum')
  result <- check_function_names(funs, error_call_string = "example_function")

  expect_equal(names(result), c("sum", "sum_1")) # Check if names are made unique

})
