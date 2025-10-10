test_that("count_char_occurrences works with vector input", {
  expect_equal(count_char_occurrences(c("hello world", "R is great"), "l"), c(3, 0))
  expect_equal(count_char_occurrences(c("data analysis", "statistics", "programming"), "a"), c(4, 1, 1))
  expect_equal(count_char_occurrences(c("no matches", "still no matches"), "z"), c(0, 0))
  expect_equal(count_char_occurrences(character(0), "a"), integer(0))
})

test_that("count_char_occurrences handles edge cases with vector input", {
  expect_error(count_char_occurrences(123, "a"), "'str_vector' must be a character vector")
  expect_error(count_char_occurrences(c("hello", "world"), "ab"), "'char' must be a single character")
})
