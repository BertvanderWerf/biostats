#' Check and Complete Names in Input
#'
#' This function checks and completes the names in the input according to the values in 'names'
#' with sensible error checks.
#'
#' @param input A character vector of names or starting strings of names.
#' @param names A character vector to check the input against.
#' @param error_call_string The parameter name used in error messages.
#' @param partial_match character string; possible values c('Anywhere','Start','Exact'). Default is 'Anywhere'.
#' @param ignore_case Logical; if TRUE, ignore case when matching. Default is FALSE.
#' @param return_index Logical; if TRUE, return indices instead of names. Default is FALSE.
#'
#' @return A named character vector of matched names or their indices.
#'
#' @export
#'
#' @examples
#' check_names(c(A='ab', 'def'), c('abc', 'defgh'))
#' check_names(c('SEP', 'PET'), c('Sepal.Length', 'Petal.Width'), ignore_case=TRUE)
#'
#'
#' names <- c("abc", "def", "ab","bac", "abcd","aq","dqp")
#' input <- c(A="ab","def")
#' check_names(input, names, "ctable(..., rows = , ...)", partial=TRUE, ignore_case=FALSE, return_index=TRUE)
#' check_names(input, names, "'Rows' of function 'ctable'", partial=FALSE, ignore_case=FALSE, return_index=FALSE)
check_names <- function(input, names, error_call_string, partial_match = NULL, ignore_case = FALSE, return_index = FALSE) {
  # Check if input is NULL
  if (is.null(input)) return(NULL)

  partial_match <- match.arg(partial_match, c('Anywhere','Start','Exact'))

  # Validate input type and content
  if (!is.character(input) || length(input) == 0) {
    stop(sprintf("%s: must be NULL or consist of character strings", error_call_string), call.=FALSE)
  }
  if (any(nchar(input) == 0 | is.na(input))) {
    stop(sprintf("%s: can't have empty or NA strings", error_call_string), call.=FALSE)
  }

  # Helper function to check individual names
  checkname <- function(name, names) {
    if (ignore_case) {
      name <- tolower(name)
      names <- tolower(names)
    }
    if (partial_match=='Exact') {
      matches <- name == names
    } else {
      if (partial_match=='Start') {
        matches <- startsWith(names, name)
      } else if (partial_match=='Anywhere') {
        matches <- grepl(name, names, fixed=TRUE)
      }
      exact_matches <- name == names
      if (any(exact_matches)) {
        matches <- matches & exact_matches
      }
    }
    return(which(matches))
  }

  # Apply checkname to all inputs
  res <- lapply(input, checkname, names)

  # Check for no matches and multiple matches
  match_counts <- lengths(res)

  no_matches <- match_counts == 0
  if (any(no_matches)) {
    not_found <- input[no_matches]
    stop(sprintf("%s: %s not found",
                 error_call_string,
                 paste(sprintf("'%s'", not_found), collapse = ", ")), call.=FALSE)
  }

  multiple_matches <- match_counts > 1
  if (any(multiple_matches)) {
    ambiguous <- input[multiple_matches]
    ambiguous_matches <- res[multiple_matches]
    error_msg <- sprintf("%s: The following inputs match multiple variables:\n", error_call_string)
    for (i in seq_along(ambiguous)) {
      error_msg <- paste0(error_msg, sprintf("  '%s' matches: %s\n",
                                             ambiguous[i],
                                             paste(names[ambiguous_matches[[i]]], collapse = ", ")))
    }
    stop(error_msg, call.=FALSE)
  }

  # Unlist results
  res <- unlist(res)

  # Handle naming of results
  if (!is.null(names(input))) {
    empty_names <- names(input) == ''
    names(res)[empty_names] <- names[res[empty_names]]
    names(res)[!empty_names] <- names(input)[!empty_names]
  } else {
    names(res) <- names[res]
  }

  # Return either indices or names based on return_index parameter
  if (!return_index) {
    res[] <- names[res]
  }
  return(res)
}

