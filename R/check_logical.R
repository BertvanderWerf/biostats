#' check_logical
#' Check and Validate Logical Arguments
#'
#' This function checks if the provided argument is a valid logical value.
#' If the argument is NULL, it returns the default value. Otherwise, it
#' performs various checks to ensure the argument is a single logical value.
#'
#' @param arg The argument to check. Should be a logical value or NULL.
#' @param default The default value to return if `arg` is NULL.
#' @param error_call_string A string to prepend to error messages for context.
#'
#' @return The validated logical value or the default value.
#'
#' throws An error if the argument is not NULL and fails any of the checks.
#'
#' @examples
#' check_logical(TRUE, FALSE, "example_function")
#' check_logical(NULL, FALSE, "example_function")
#' \dontrun{
#'   check_logical(c(TRUE, FALSE), FALSE, "example_function")  # Will throw an error
#'   check_logical(NA, FALSE, "example_function")  # Will throw an error
#' }
#'
#' @export
check_logical <- function(arg, default, error_call_string) {
  if (is.null(arg)) {
    return(default)
  }

  if (!is.logical(arg) || length(arg) == 0) {
    stop(
      sprintf("%s: must be NULL or a single logical value (TRUE or FALSE)", error_call_string),
      call. = FALSE
    )
  }

  if (length(arg) != 1) {
    stop(
      sprintf("%s: must have exactly one value", error_call_string),
      call. = FALSE
    )
  }

  if (is.na(arg)) {
    stop(
      sprintf("%s: cannot be NA", error_call_string),
      call. = FALSE
    )
  }

  arg
}

#' check_characterstring
#' Check and Validate Logical Arguments
#'
#' This function checks if the provided argument is a valid character string.
#' If the argument is NULL, it returns the default value. Otherwise, it
#' performs various checks to ensure the argument is a single character string value.
#' NA will be translated to the string 'NA'
#'
#' @param arg The argument to check. Should be a single character string value or NULL.
#' @param default The default value to return if `arg` is NULL, default NULL.
#' @param error_call_string A string to prepend to error messages for context.
#'
#' @return The validated character string value or the default value.
#'
#' throws An error if the argument is not NULL and fails any of the checks.
#'
#' @examples
#' check_characterstring('abc', 'ABC', "example_function")
#' check_characterstring(NULL, 'ABC', "example_function")
#' \dontrun{
#'   check_characterstring(c('ABC', 'FALSE'), NULL, "example_function")  # Will throw an error
#'   check_characterstring(NA, "example_function")  # Will return 'NA'
#' }
#'
#' @export
check_characterstring <- function(arg, default=NULL, error_call_string) {
  if (is.null(arg)) {
    return(default)
  }

  if (!is.character(arg) || length(arg) == 0) {
    stop(
      sprintf("%s: must be NULL or a single character string value", error_call_string),
      call. = FALSE
    )
  }

  if (length(arg) != 1) {
    stop(
      sprintf("%s: must have exactly one value", error_call_string),
      call. = FALSE
    )
  }

  if (is.na(arg)) {
    arg <- 'NA'
  }

  arg
}
