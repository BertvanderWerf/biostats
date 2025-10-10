#' Apply a Function to List-like Objects with Empty and NA Handling
#'
#' This function applies a given function to all elements of a list-like object,
#' handling empty elements and NA values according to the specified parameters.
#'
#' @param data_i A list-like object to which the function will be applied.
#' @param f The function to be applied to each element of \code{data_i}.
#' @param na.rm Logical; if TRUE, NA values are removed before applying \code{f}.
#' @param ... Additional arguments to be passed to \code{f}.
#'
#' @return A list with the same structure as \code{data_i}, where \code{f} has been
#'         applied to each element. Empty elements or those that become empty
#'         after NA removal (if \code{na.rm = TRUE}) are replaced with NA.
#'
#' @examples
#' data <- list(a = 1:3, b = numeric(0), c = c(NA, 2, 3))
#' rapply_function(data, mean)
#' rapply_function(data, sum, na.rm = FALSE)
#'
#' @export
rapply_function <- function(data_i, f, na.rm = TRUE, ...) {
  f_args <- names(formals(f))

  handle_empty <- function(x) {
    if (na.rm == TRUE) x <- x[!is.na(x)]
    if (length(x) == 0) NA else f(x, ...)
  }

  if (!('...' %in% f_args)) {
    handle_empty <- function(x) {
      if (na.rm == TRUE) x <- x[!is.na(x)]
      if (length(x) == 0) NA else f(x)
    }
  }

  return(rapply(data_i, f = handle_empty, how = 'list'))
}
