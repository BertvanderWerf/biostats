#' check_string_vector
#'
#' Check a Character String Vector
#'
#' Validates that an input is a character vector and meets specified constraints,
#' including length, naming, NULL/NA/empty string handling, and uniqueness.
#'
#' @param arg An argument to check for being a character vector.
#' @param arg_name The name of the argument to report in error messages.
#' @param arg_default An optional default value if \code{arg} is NULL and allowed.
#' @param arg_length Optional expected length for the vector.
#' @param arg_names Logical; whether the vector should have names assigned and unique.
#' @param arg_null Logical; whether \code{arg} is allowed to be NULL.
#' @param arg_na Logical; whether NA/NaN values are allowed.
#' @param arg_empty Logical; whether "" values are allowed.
#' @param arg_zero Logical; whether a zero-length character vector (character(0)) is allowed.
#'
#' @return A validated character vector (possibly assigned names) suitable for downstream use.
#'
#' @examples
#' check_string_vector(c("a", "b"), "letters")
#' check_string_vector(NULL, "empty", arg_default = c("x", "y"), arg_null = TRUE)
#' @export
check_string_vector <- function(arg, arg_name, arg_default=NULL, arg_length=NULL,
                                arg_names=FALSE, arg_null=FALSE, arg_na=FALSE,
                                arg_empty=FALSE, arg_zero=FALSE) {
  # Argument self-validation
  if (!is.character(arg_name) || length(arg_name) != 1) {
    stop("Argument 'arg_name' must be a single character string.", call. = FALSE)
  }
  if (!is.null(arg_length) && (!is.numeric(arg_length) || length(arg_length) != 1)) {
    stop("Argument 'arg_length' must be a single numeric value or NULL.", call. = FALSE)
  }
  if (!is.logical(arg_names) || length(arg_names) != 1) {
    stop("Argument 'arg_names' must be a single logical value.", call. = FALSE)
  }
  if (!is.logical(arg_null) || length(arg_null) != 1) {
    stop("Argument 'arg_null' must be a single logical value.", call. = FALSE)
  }
  if (!is.logical(arg_na) || length(arg_na) != 1) {
    stop("Argument 'arg_na' must be a single logical value.", call. = FALSE)
  }
  if (!is.logical(arg_empty) || length(arg_empty) != 1) {
    stop("Argument 'arg_empty' must be a single logical value.", call. = FALSE)
  }
  if (!is.logical(arg_zero) || length(arg_zero) != 1) {
    stop("Argument 'arg_zero' must be a single logical value.", call. = FALSE)
  }

  # Get calling function for error reporting
  calling_function_name <- deparse(sys.call(sys.parent())[[1]])

  # NULL handling
  if (is.null(arg) && !arg_null) {
    if (is.null(arg_default)) {
      stop(sprintf("Argument '%s' in function '%s' must not be NULL.",
                   arg_name, calling_function_name), call. = FALSE)
    } else {
      arg <- check_string_vector(arg_default, 'arg_default', arg_length=arg_length,
                                 arg_names=arg_names, arg_null=arg_null, arg_na=arg_na, arg_empty=arg_empty)
    }
  }

  # Type check
  if (!is.null(arg) && !is.character(arg)) {
    stop(sprintf("Argument '%s' in function '%s' must be a character vector.", arg_name,
                 calling_function_name), call. = FALSE)
  }

  # NA check
  if (!is.null(arg) && any(is.na(arg)) && !arg_na) {
    stop(sprintf("Argument '%s' in function '%s' must not contain NA/NaN.", arg_name,
                 calling_function_name), call. = FALSE)
  }

  # Empty string check
  if (!is.null(arg) && any(arg == "") && !arg_empty) {
    stop(sprintf("Argument '%s' in function '%s' must not contain empty strings ('').",
                 arg_name, calling_function_name), call. = FALSE)
  }

  # Length check
  if (!is.null(arg_length) && length(arg) != arg_length) {
    if (arg_length == 1) {
      stop(sprintf("Argument '%s' in function '%s' must be a single character string.",
                   arg_name, calling_function_name), call. = FALSE)
    }
    stop(sprintf("Argument '%s' in function '%s' must be of length %s.",
                 arg_name, calling_function_name, arg_length), call. = FALSE)
  }

  if (!is.null(arg) && length(arg) == 0 && !arg_zero) {
    stop(sprintf("Argument '%s' in function '%s' must not be a zero-length character vector.",
                 arg_name, calling_function_name), call. = FALSE)
  }

  # Name assignment and duplication check
  if (!is.null(arg) && arg_names) {
    if (is.null(names(arg))) {
      names(arg) <- arg
    } else {
      cond_missing <- names(arg) == ""
      names(arg)[cond_missing] <- arg[cond_missing]
    }
    n_names <- table(names(arg))
    cond_multiple <- n_names > 1
    if (any(cond_multiple)) {
      names_multiple <- names(n_names)[cond_multiple]
      msg_names <- paste(names_multiple, collapse = "', '")
      if (length(names_multiple) == 1) {
        stop(sprintf("Argument '%s' in function '%s': the name '%s' occurs multiple times.",
                     arg_name, calling_function_name, msg_names), call. = FALSE)
      } else {
        stop(sprintf("Argument '%s' in function '%s': the names ('%s') occur multiple times.",
                     arg_name, calling_function_name, msg_names), call. = FALSE)
      }
    }
  }
  arg
}
