
test_that("basic checks succeed", {
  expect_equal(check_string_vector(c("a", "b"), "myvec"), c("a", "b"))
})

test_that("NULL with default allowed", {
  expect_equal(check_string_vector(NULL, "test", arg_default=c("x", "y"), arg_null=TRUE), c("x", "y"))
})

test_that("rejects non-character-type", {
  expect_error(check_string_vector(1:3, "notchar"), "must be a character vector")
})

test_that("rejects NA when not allowed", {
  expect_error(check_string_vector(c("a", NA), "vecna"), "must not contain NA/NaN")
})

test_that("rejects wrong length", {
  expect_error(check_string_vector(c("a", "b"), "vec", arg_length=1), "must be of length")
})

test_that("detects duplicate names", {
  v <- c("A", "B", "A"); names(v) <- c("foo", "foo", "bar")
  expect_error(check_string_vector(v, "dupnames", arg_names=TRUE), "non-unique names")
})

test_that("assigns names if missing", {
  v <- c("one", "two")
  expect_equal(names(check_string_vector(v, "foo", arg_names=TRUE)), c("one", "two"))
})

test_that("empty strings handled", {
  expect_error(
    check_string_vector(c("a", ""), "test", arg_empty = FALSE),
    "must not contain empty strings"
  )
  expect_equal(
    check_string_vector(c("a", ""), "test", arg_empty = TRUE),
    c("a", "")
  )
})

test_that("type of arg_length must be numeric", {
  expect_error(
    check_string_vector("a", "test", arg_length = "bad"),
    "arg_length' must be a single numeric value"
  )
})

test_that("all checks pass for proper usage", {
  expect_equal(
    check_string_vector(c("red", "green"), "colours", arg_length = 2, arg_names = TRUE, arg_empty = TRUE),
    c("red", "green")
  )
})
