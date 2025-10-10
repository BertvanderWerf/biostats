#' Count Character Occurrences in String Vector
#'
#' This function counts the number of occurrences of a specified character in each string of a given vector.
#'
#' @param str_vector A character vector of strings to search for occurrences.
#' @param char A single character to count within each string.
#'
#' @return A numeric vector representing the number of occurrences of the character in each string.
#'
#' @examples
#' count_char_occurrences(c("hello world", "R is great"), "l")
#' count_char_occurrences(c("data analysis", "statistics", "programming"), "a")
#'
#' @export
count_char_occurrences <- function(str_vector, char) {
  if (!is.character(str_vector)) {
    stop("'str_vector' must be a character vector")
  }
  if (!is.character(char) || nchar(char) != 1) {
    stop("'char' must be a single character")
  }


  as.integer(sapply(regmatches(str_vector, gregexpr(char, str_vector, fixed=TRUE)), length))
}
