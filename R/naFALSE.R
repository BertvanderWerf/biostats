
#' naFALSE
#'
#' @description
#' Treat NA as FALSE
#'
#' @param x A logical vector
#'
#' @return A logical vector where NA is treated as FALSE
#' @export
#'
#' @examples
#' naFALSE(c(NA, FALSE, TRUE))
naFALSE <- function(x) {
  if (is.null(x)) return(NULL)
  if (!is.logical(x)) stop('Input must be logical')
  (!is.na(x)) & x
}
