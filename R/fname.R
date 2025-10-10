#' Get the 'fname' attribute of an object
#'
#' This function retrieves the 'fname' attribute of an object.
#' It is particularly useful for objects of class 'ctable'.
#'
#' @param x An R object
#'
#' @return The value of the 'fname' attribute if it exists, otherwise NULL
#'
#' @export
fname <- function(x) {
  UseMethod("fname")
}

#' @rdname fname
#' @export
fname.default <- function(x) {
  attr(x, "fname", exact = TRUE)
}

#' @rdname fname
#' @export
fname.ctable <- function(x) {
  fname <- attr(x, "fname", exact = TRUE)
  if (is.null(fname)) {
    warning("'ctable' object does not have a 'fname' attribute")
  }
  fname
}

#' Set the 'fname' attribute of an object
#'
#' This function sets the 'fname' attribute of an object.
#' It is particularly useful for objects of class 'ctable'.
#'
#' @param x An R object
#' @param value A character string to set as the new 'fname' value
#'
#' @return The modified object with updated 'fname' attribute
#'
#' @export
`fname<-` <- function(x, value) {
  UseMethod("fname<-")
}

#' @rdname fname
`fname<-.default` <- function(x, value) {
  if (!is.character(value) || length(value) != 1) {
    stop("'value' must be a single character string")
  }
  attr(x, "fname") <- value
  x
}

#' @rdname fname
#' fname<-.ctable
#'
#' @description
#' #' Set the 'fname' attribute of a ctable object
#'
#' This function sets the 'fname' attribute of a ctable object and updates
#' the corresponding column name if it exists.
#'
#' @param x A ctable object
#' @param value A character string to set as the new 'fname' value
#'
#' @return The modified ctable object with updated 'fname' attribute and possibly updated column name
#'
#' @details
#' This function performs the following steps:
#' 1. Validates that the new value is a single character string.
#' 2. Checks if the old 'fname' exists as a column name.
#' 3. If it does, it ensures the new name doesn't conflict with existing column names.
#' 4. Updates the column name if necessary.
#' 5. Sets the new 'fname' attribute.
#'
#' @examples
#' \dontrun{
#' ct <- ctable(data.frame(x = 1:3, y = 4:6))
#' fname(ct) <- "new_name"
#' }
#'
#' @seealso \code{\link{fname}}
#'
#' @export
`fname<-.ctable` <- function(x, value) {
  if (!is.character(value) || length(value) != 1) {
    stop("'value' must be a single character string", call. = FALSE)
  }
  if (attr(x, 'fname') ==value) {
    stop("value can't be equal to the fname attribute of x", call. = FALSE)
  }

  old_name <- fname(x)
  if (old_name==value) {
    return(x)
  }

  if (!is.null(old_name)) {
    if (old_name %in% names(cols(x))) {
      if (value %in% names(cols(x))) {
        stop(sprintf("The new fname '%s' is already a column variable in the ctable", value), call. = FALSE)
      }
      names(attr(x, 'cols'))[names(cols(x)) == old_name] <- value
      hsep <- attr(x, 'hsep')
      base::names(x) <- c(names(rows(x)), header_to_column_names(attr(x, 'cols'), hsep[1], hsep[2], header_minimal=FALSE))
    }
  }

  attr(x, "fname") <- value
  class(attr(x, 'fname')) <- c('ctable_fname', class(attr(x, 'fname')))

  if (is_valid_ctable(x)) {
    return(x)
  }
  stop('restult is not a valid ctable')
}
