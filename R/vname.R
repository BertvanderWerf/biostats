#' vname
#'
#' Get the 'vname' attribute of an object
#'
#' This function retrieves the 'vname' attribute of an object.
#' It is particularly useful for objects of class 'ctable'.
#'
#' @param x An R object
#'
#' @return The value of the 'vname' attribute if it exists, otherwise NULL
#'
#' @export
vname <- function(x) {
  UseMethod("vname")
}

#' @rdname vname
#' @export
vname.default <- function(x) {
  attr(x, "vname", exact = TRUE)
}

#' @rdname vname
#' @export
vname.ctable <- function(x) {
  vname <- attr(x, "vname", exact = TRUE)
  if (is.null(vname)) {
    warning("'ctable' object does not have a 'vname' attribute")
  }
  vname
}

#' vname<-
#'
#' @description
#' Set the 'vname' attribute of an object
#'
#' This function sets the 'vname' attribute of an object.
#' It is particularly useful for objects of class 'ctable'.
#'
#' @param x An R object
#' @param value A character string to set as the new 'vname' value
#'
#' @return The modified object with updated 'vname' attribute
#'
#' @export
`vname<-` <- function(x, value) {
  UseMethod("vname<-")
}

#' @rdname vname
#' @export
`vname<-.default` <- function(x, value) {
  if (!is.character(value) || length(value) != 1) {
    stop("'value' must be a single character string")
  }
  attr(x, "vname") <- value
  x
}

#' @rdname vname
#' vname<-.ctable
#'
#' @description
#' Set the 'vname' attribute of a ctable object
#'
#' This function sets the 'vname' attribute of a ctable object and updates
#' the corresponding column name if it exists.
#'
#' @param x A ctable object
#' @param value A character string to set as the new 'vname' value
#'
#' @return The modified ctable object with updated 'vname' attribute and possibly updated column name
#'
#' @details
#' This function performs the following steps:
#' 1. Validates that the new value is a single character string.
#' 2. Checks if the old 'vname' exists as a column name.
#' 3. If it does, it ensures the new name doesn't conflict with existing column names.
#' 4. Updates the column name if necessary.
#' 5. Sets the new 'vname' attribute.
#'
#' @examples
#' \dontrun{
#' ct <- ctable(data.frame(x = 1:3, y = 4:6))
#' vname(ct) <- "new_name"
#' }
#'
#' @seealso \code{\link{vname}}
#'
#' @export
`vname<-.ctable` <- function(x, value) {
  if (!is.character(value) || length(value) != 1) {
    stop("'value' must be a single character string", call. = FALSE)
  }
  if (attr(x, 'fname') ==value) {
    stop("value can't be equal to the fname attribute of x", call. = FALSE)
  }

  old_name <- vname(x)
  if (old_name==value) {
    return(x)
  }

  if (!is.null(old_name)) {
    if (old_name %in% names(rows(x))) {
      if (value %in% names(rows(x))) {
        stop(sprintf("The new vname '%s' is already a column name in the ctable", value), call. = FALSE)
      }
      names(attr(x, 'rows'))[names(rows(x)) == old_name] <- value
      base::names(x)[names(x)==old_name] <- value
    }
  }

  attr(x, "vname") <- value
  class(attr(x, 'vname')) <- c('ctable_vname', class(attr(x, 'vname')))

  if (is_valid_ctable(x)) {
    return(x)
  }
  stop('result is not a valid ctable')

}
