#' Get the 'rows' attribute of an object
#'
#' This function retrieves the 'rows' attribute of an object.
#' It is particularly useful for objects of class 'ctable'.
#'
#' @param x An R object
#'
#' @return The value of the 'rows' attribute if it exists, otherwise NULL
#'
#' @export
rows <- function(x) {
  UseMethod("rows")
}

#' @rdname rows
#' @export
rows.default <- function(x) {
  attr(x, "rows", exact = TRUE)
}

#' @rdname rows
#' @export
rows.ctable <- function(x) {
  rows <- attr(x, "rows", exact = TRUE)
  if (is.null(rows)) {
    warning("'ctable' object does not have a 'rows' attribute")
  }
  rows
}

#' rows<-
#'
#' @description
#' Set the 'rows' attribute of an object
#'
#' This function sets the 'rows' attribute of an object.
#' It is particularly useful for objects of class 'ctable'.
#'
#' @param x An R object
#' @param value A data.frame to set as the new 'rows' value
#'
#' @return The modified object with updated 'rows' attribute
#'
#' @export
`rows<-` <- function(x, value) {
  UseMethod("rows<-")
}

#' @rdname rows
#' @export
`rows<-.default` <- function(x, value) {
  if (!is.data.frame(value) ) {
    stop("'value' must be a data.frame")
  }
  attr(x, "rows") <- value
  x
}

#' @rdname rows
#' rows<-
#'
#' @description
#' Set the 'rows' attribute of a ctable object
#'
#' This function sets the 'rows' attribute of a ctable object and updates
#' the corresponding column name(s) if it exists.
#'
#' @param x A ctable object
#' @param value A data.frame to set as the new 'rows' value
#' @param vname the variable name default NULL: vname(x)
#'
#' @return The modified ctable object with updated 'rows' attribute and possibly updated column name
#'
#' @details
#' This function performs the following steps:
#' 1. Validates that the new value is a data.frame string.
#' 2. Checks if the old 'rows' has equal number of rows.
#' 3. If it does, it ensures the new name doesn't conflict with existing column names.
#' 4. Updates the column name if necessary.
#' 5. Sets the new 'rows' attribute.
#'
#' @examples
#' \dontrun{
#' ct <- ctable(data.frame(x = 1:3, y = 4:6))
#' rows(ct) <- "new_name"
#' }
#'
#' @seealso \code{\link{rows}}
#'
#' @export
`rows<-.ctable` <- function(x, value, vname=NULL) {
  old_rows <- rows(x)
  if (is.null(vname)) vname <- vname(x)

  if (!is.data.frame(value) || nrow(value) != nrow(old_rows)) {
    stop("'value' must be a data.frame with nrows equal to nrows of rows(x)", call. = FALSE)
  }
  if (!(vname %in% names(value))) {
    stop(sprintf("the vname: '%s' should be a columnname in the input data.frame value", vname))
    vname(x) <- vname
  }
  if (!is_unique_key(value, names(value))) {
    stop("the new rows(x) should define a key into the table, combinations are not unique")
  }

  x[,names(old_rows)] <- value

  attr(x, "rows") <- value
  class(attr(x, 'rows')) <- c('ctable_rows', class(attr(x, 'rows')))

  if (is_valid_ctable(x, FALSE)) {
    return(x)
  }
}
