#' @noRd
#' helper function for check_function_names
set_str_names <- function(x) {
  if (is.null(x)) return(NULL)
  if (!is.character(x)) stop('argument x must be of type character')
  if (is.null(names(x))) {
    names(x) <- x
  } else {
    unnamed <- names(x)=='' | is.na(names(x))
    names(x)[unnamed] <- x[unnamed]
  }
  x
}

#' Check and Process Function Names
#'
#' This function validates and processes a vector of function names, which may include
#' package names in the format 'package_name::function_name' or just 'function_name'.
#' It returns a list of the actual function objects.
#'
#' @param fun A character vector of function names, or NULL.
#' @param default The default function name to use if `fun` is NULL. Can include package name (e.g., 'base::sum'). Default is 'base::sum'.
#' @param error_call_string A string to prepend to error messages for context.
#' @param envir environment to check function default .GlobalEnv
#'
#' @return A named list of function objects.
#'
#' throws An error if the input is invalid or if any function cannot be found.
#'
#' @examples
#' check_function_names(c('base::mean', 'median', 'base::sum'), error_call_string = "example_function")
#' check_function_names(NULL, error_call_string = "example_function")
#' check_function_names(NULL, default = 'stats::median', error_call_string = "example_function")
#' \dontrun{
#'   check_function_names(c('', 'mean'), error_call_string = "example_function")  # Will throw an error
#'   check_function_names(c(NA, 'sum'), error_call_string = "example_function")   # Will throw an error
#' }
#'
#' @export
check_function_names <- function(fun, default = 'count', error_call_string, envir = .GlobalEnv) {
  if (is.null(fun)) {
    default <- set_str_names(default)
    return(stats::setNames(list(get(default)), default))
  }

  if (!is.character(fun) || length(fun) == 0) {
    stop(sprintf("%s: must be NULL or a non-empty character vector", error_call_string), call. = FALSE)
  }

  if (any(nchar(fun) == 0 | is.na(fun))) {
    stop(sprintf("%s: cannot contain empty or NA strings", error_call_string), call. = FALSE)
  }

  # Split package and function names
  split_names <- strsplit(fun, "::")
  packages <- vapply(split_names, function(x) if(length(x) > 1) x[1] else "", character(1))
  functions <- vapply(split_names, function(x) x[length(x)], character(1))

  # Set names for the function vector
  if (is.null(names(fun))) {
    names(functions) <- functions
  } else {
    unnamed <- names(fun) == '' | is.na(names(fun))
    names(functions)[unnamed] <- functions[unnamed]
  }

  # Create a list of function objects
  result <- vector("list", length(functions))
  for (i in seq_along(functions)) {
    tryCatch({
      if (packages[i] == '') {
        result[[i]] <- get(functions[i], envir = envir)
      } else {
        result[[i]] <- getExportedValue(packages[i], functions[i])
      }
    }, error = function(e) {
      stop(sprintf("%s: function '%s' not found", error_call_string, fun[i]), call. = FALSE)
    })
    if (!is.function(result[[i]])) {
      stop(sprintf("%s: '%s' is not a function", error_call_string, fun[i]), call. = FALSE)
    }
  }

  # Make names unique
  names(result) <- make.unique(names(functions), sep = '_')

  return(result)
}
