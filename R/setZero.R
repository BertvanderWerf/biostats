
#' setZero
#'
#' @param x a data structure in r
#'
#' @return any x value is changed if abs(x_value) <= TOL
#' @export
#'
#' @examples
#' x <- setZero(x)
setZero <- function(x) {
  x[abs(x)<=2^-43] <- 0
  x
}
