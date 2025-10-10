#' Get the 'hsep' attribute of an object
#'
#' This function retrieves the 'hsep' attribute of an object.
#' It is particularly useful for objects of class 'ctable'.
#'
#' @param x An R object
#'
#' @return The value of the 'hsep' attribute if it exists, otherwise NULL
#'
#' @export
hsep <- function(x) {
  UseMethod("hsep")
}

#' @rdname hsep
#' @export
hsep.default <- function(x) {
  attr(x, "hsep", exact = TRUE)
}

#' @rdname hsep
#' @export
hsep.ctable <- function(x) {
  hsep <- attr(x, "hsep", exact = TRUE)
  if (is.null(hsep)) {
    warning("'ctable' object does not have a 'hsep' attribute")
  }
  hsep
}

#' Set the 'hsep' attribute of an object
#'
#' This function sets the 'hsep' attribute of an object.
#' It is particularly useful for objects of class 'ctable'.
#'
#' @param x An R object
#' @param value A character string to set as the new 'hsep' value
#'
#' @return The modified object with updated 'hsep' attribute
#'
#' @export
`hsep<-` <- function(x, value) {
  UseMethod("hsep<-")
}

#' @rdname hsep
#' @export
`hsep<-.default` <- function(x, value) {
  if (!((is.null(value) || !is.character(value) || (2 != length(value)) || all(nchar(value)==1)))) {
    if (isTRUE(messages)) {
      stop(sprintf("the attribute 'hsep' must be a character vector of length 2 each having one character"))
    }
  }
  attr(x, "hsep") <- value
  x
}

#' @rdname hsep
#' hsep<-.ctable
#'
#' @description
#' Set the 'hsep' attribute of a ctable object
#'
#' This function sets the 'hsep' attribute of a ctable object and updates
#' the corresponding column name if it exists.
#'
#' @param x A ctable object
#' @param value A character string to set as the new 'hsep' value
#'
#' @return The modified ctable object with updated 'hsep' attribute and possibly updated column name
#'
#' @details
#' This function performs the following steps:
#' 1. Validates that the new value is a single character string.
#' 2. Checks if the old 'hsep' exists as a column name.
#' 3. If it does, it ensures the new name doesn't conflict with existing column names.
#' 4. Updates the column name if necessary.
#' 5. Sets the new 'hsep' attribute.
#'
#' @examples
#' \dontrun{
#' ct <- ctable(data.frame(x = 1:3, y = 4:6))
#' hsep(ct) <- "new_name"
#' }
#'
#' @seealso \code{\link{hsep}}
#'
#' @export
`hsep<-.ctable` <- function(x, value) {
  if (!((is.null(value) || !is.character(value) || (2 != length(value)) || all(nchar(value)==1)))) {
    if (isTRUE(messages)) {
      stop(sprintf("the attribute 'hsep' must be a character vector of length 2 each having one character"))
    }
  }

  if (fname(x) %in% value) {
    stop("value can't be equal to the fname attribute of x", call. = FALSE)
  }
  if (vname(x) %in% value) {
    stop("value can't be equal to the vname attribute of x", call. = FALSE)
  }

  if (all_equal(value)) {
    stop("hsep can't have two equal values")
  }

  old_hsep <- hsep(x)
  if (all(old_hsep == value)) {
    return(x)
  }

  cols <- cols(x)
  for (i in 1:2) {
    if (old_hsep[i]!=value[i]) {
      cond <- sapply(cols, function(x) any(grepl(value[i], x, fixed=TRUE)))
      if (any(cond)) {
        stop(sprintf("The proposed value of %c for hsep[%i] is not valid, character already exists in variable %s",
                     value[i], i, paste(names(cols)[cond], collapse="', '")))
      }
      cond <- sapply(names(cols), function(x) any(grepl(value[i], x, fixed=TRUE)))
      if (any(cond)) {
        stop(sprintf("The proposed value of %c for hsep[%i] is not valid, character already exists in variable name %s",
                     value[i], i, paste(names(cols)[cond], collapse="', '")))
      }
    }
  }

  attr(x, "hsep") <- value
  class(attr(x, 'hsep')) <- c('ctable_hsep', class(attr(x, 'hsep')))

  base::names(x) <- c(names(rows(x)), header_to_column_names(cols(x), value[1], value[2], header_minimal=FALSE))
  if (is_valid_ctable(x)) {
    return(x)
  }
  stop('result is not a valid ctable')

}
