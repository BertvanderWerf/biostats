#' Check and Validate Integer Arguments
#'
#' This function checks if the provided argument is a valid integer value.
#' If the argument is NULL, it returns the default value. Otherwise, it
#' performs various checks to ensure the argument is a single integer value.
#'
#' @param arg The argument to check. Should be an integer value or NULL.
#' @param default The default value to return if `arg` is NULL.
#' @param error_call_string A string to prepend to error messages for context.
#' @param allow_na Logical; whether to allow NA as a valid value. Default is FALSE.
#' @param min Numeric; the minimum allowed value (inclusive). Default is -Inf.
#' @param max Numeric; the maximum allowed value (inclusive). Default is Inf.
#'
#' @return The validated integer value or the default value.
#'
#' throws An error if the argument is not NULL and fails any of the checks.
#'
#' @examples
#' check_integer(5, 0, "example_function")
#' check_integer(NULL, 0, "example_function")
#' check_integer(3, 0, "example_function", min = 1, max = 10)
#' \dontrun{
#'   check_integer(3.5, 0, "example_function")  # Will throw an error
#'   check_integer(c(1, 2), 0, "example_function")  # Will throw an error
#'   check_integer(NA, 0, "example_function")  # Will throw an error
#' }
#'
#' @export
check_integer <- function(arg, default, error_call_string, allow_na = FALSE, min = 0, max = .Machine$integer.max) {
  if (is.null(arg)) {
    return(default)
  }

  if (!is.numeric(arg) || length(arg) == 0) {
    stop(
      sprintf("%s: must be NULL or a single integer value", error_call_string),
      call. = FALSE
    )
  }

  if (length(arg) != 1) {
    stop(
      sprintf("%s: must have exactly one value", error_call_string),
      call. = FALSE
    )
  }

  if (is.na(arg) && !allow_na) {
    stop(
      sprintf("%s: cannot be NA", error_call_string),
      call. = FALSE
    )
  }

  if (!is.na(arg) && !isTRUE(all.equal(arg, as.integer(arg)))) {
    stop(
      sprintf("%s: must be an integer value", error_call_string),
      call. = FALSE
    )
  }

  if (!is.na(arg) && (arg < min || arg > max)) {
    stop(
      sprintf("%s: must be between %s and %s", error_call_string, min, max),
      call. = FALSE
    )
  }

  as.integer(arg)
}
