#' Get the 'cols' attribute of an object
#'
#' This function retrieves the 'cols' attribute of an object.
#' It is particularly useful for objects of class 'ctable'.
#'
#' @param x An R object
#'
#' @return The value of the 'cols' attribute if it exists, otherwise NULL
#'
#' @export
cols <- function(x) {
  UseMethod("cols")
}

#' @rdname cols
#' @export
cols.default <- function(x) {
  attr(x, "cols", exact = TRUE)
}

#' @rdname cols
#' @export
cols.ctable <- function(x) {
  cols <- attr(x, "cols", exact = TRUE)
  if (is.null(cols)) {
    warning("'ctable' object does not have a 'cols' attribute")
  }
  cols
}


#' cols<-
#'
#' @description
#'
#' This function sets the 'cols' attribute of an object.
#' It is particularly useful for objects of class 'ctable'.
#'
#' @param x An R object
#' @param value A data.frame to set as the new 'cols' value
#'
#' @return The modified object with updated 'cols' attribute
#'
#' @export
`cols<-` <- function(x, value) {
  UseMethod("cols<-")
}

#' @rdname cols
#' @export
`cols<-.default` <- function(x, value) {
  if (!is.data.frame(value) ) {
    stop("'value' must be a data.frame")
  }
  attr(x, "cols") <- value
  x
}

#' @rdname cols
#' cols<-.ctable
#'
#' @description
#' Set the 'cols' attribute of a ctable object
#'
#' This function sets the 'cols' attribute of a ctable object and updates
#' the corresponding column name(s) if it exists.
#'
#' @param x A ctable object
#' @param value A data.frame to set as the new 'cols' value
#'
#' @return The modified ctable object with updated 'cols' attribute and possibly updated column name
#'
#' @details
#' This function performs the following steps:
#' 1. Validates that the new value is a data.frame string.
#' 2. Checks if the old 'cols' has equal number of rows.
#' 3. If it does, it ensures the new name doesn't conflict with existing column names.
#' 4. Updates the column name if necessary.
#' 5. Sets the new 'cols' attribute.
#'
#' @examples
#' \dontrun{
#' ct <- ctable(data.frame(x = 1:3, y = 4:6))
#' cols(ct) <- "new_name"
#' }
#'
#' @seealso \code{\link{cols}}
#'
#' @export
`cols<-.ctable` <- function(x, value) {
  old_cols <- cols(x)
  if (!is.data.frame(value) || nrow(value) != nrow(old_cols)) {
    stop("'value' must be a data.frame with nrows equal to nrows of cols(x)", call. = FALSE)
  }

  attr(x, "cols") <- value
  class(attr(x, 'cols')) <- c('ctable_cols', class(attr(x, 'cols')))

  hsep <- attr(x, 'hsep')

  base::names(x) <- c(names(rows(x)), header_to_column_names(value, hsep[1], hsep[2],header_minimal=FALSE))
  base::names(x) <- make.unique(names(x))

  if (is_valid_ctable(x,FALSE)) {
    return(x)
  }
}
