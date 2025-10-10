#' Check if All Vector Values are Equal
#'
#' This function tests whether all elements in a given vector are equal.
#' It considers NA values as equal to each other.
#'
#' @param x A vector of any type (numeric, character, logical, etc.)
#'
#' @return A logical value: TRUE if all elements are equal (including all NAs), FALSE otherwise.
#'          Returns TRUE for vectors of length 0 or 1.
#'
#' @examples
#' all_equal(c(1, 1, 1, 1))
#' all_equal(c("a", "a", "a"))
#' all_equal(c(NA, NA, NA))
#'
#' @export
all_equal <- function(x) {
  if (length(x) <= 1) return(TRUE)
  if (all(is.na(x))) return(TRUE)
  if (any(is.na(x))) return(FALSE)
  all(x == x[1])
}

