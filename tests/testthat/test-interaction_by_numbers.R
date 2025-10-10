test_that("interaction_by_numbers works correctly", {
  # Setup test data
  f1 <- factor(c("a", "b", "a"))
  f2 <- factor(c("x", "x", "y"))
  chr <- c("high", "low", "high")
  num <- c(1, 2, 1)

  # Test basic functionality
  test1 <- interaction_by_numbers(f1, f2, drop=FALSE)
  expect_equal(levels(test1), c("1.1", "2.1", "1.2", "2.2"))
  expect_equal(attr(test1, "levels_i"), list(c("a", "b"), c("x", "y")))

  # Test mixed input types
  test2 <- interaction_by_numbers(f1, chr, drop=TRUE)
  expect_equal(levels(test2), c("1.1", "2.2"))
  expect_equal(attr(test2, "levels_i"), list(c("a", "b"), c("high", "low")))

  # Test drop = TRUE
  test3 <- interaction_by_numbers(f1[1:2], f2[1:2], drop = TRUE)
  expect_equal(nlevels(test3), 2)

  # Test lex.order
  test4 <- interaction_by_numbers(f2, f1, lex.order = TRUE)
  expect_equal(levels(test4), c("1.1", "1.2", "2.1", "2.2"))

  # Test numeric input handling
  test5 <- interaction_by_numbers(num, f1)
  expect_equal(attr(test5, "levels_i"), list(c("1", "2"), c("a", "b")))

  # Test edge case: single factor
  test6 <- interaction_by_numbers(f1)
  expect_equal(levels(test6), c("1", "2"))

  # Test empty input handling
  expect_error(interaction_by_numbers())
})
