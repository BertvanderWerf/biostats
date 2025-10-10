
#' is_unique_key
#'
#' @description
#'
#' Check if Specified Fields Form a Unique Key
#'
#' This function determines whether a set of fields in a data frame forms a unique key.
#' It creates a temporary column by combining the specified fields and checks if all
#' combinations are unique.
#'
#' @param data A data frame to check.
#' @param fields A character vector of column names to check as a potential key.
#'
#' @return A logical value. TRUE if the specified fields form a unique key,
#'   FALSE otherwise.
#'
#' @examples
#' df <- data.frame(A = 1:3, B = c("x", "y", "z"), C = c("a", "a", "b"))
#' key_fields(df, c("A", "B"))  # Returns TRUE
#' key_fields(df, c("B", "C"))  # Returns FALSE
#'
#' @export
is_unique_key <- function(data, fields) {
  # Coerce data to data.frame
  data <- as.data.frame(data)

  # Coerce fields to character vector
  fields <- as.character(fields)

  # Input validation
  if (!all(fields %in% names(data))) {
    stop("All specified fields must exist in the data")
  }

  # Create a temporary column name
  newname <- make.names(c(names(data), "temp_key"), unique = TRUE)[length(names(data)) + 1]

  # Create the temporary variable using interaction
  test <- interaction(data[, fields, drop = FALSE], drop = TRUE)

  # Check if all combinations are unique
  all(table(test) <= 1)
}

