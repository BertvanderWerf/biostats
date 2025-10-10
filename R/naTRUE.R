#' naTRUE
#'
#' @description
#' Treat NA as TRUE
#'
#' @param x A logical vector
#'
#' @return A logical vector where NA is treated as TRUE
#' @export
#'
#' @examples
#' naTRUE(c(NA, FALSE, TRUE))
naTRUE <- function(x) {
  if (is.null(x)) return(NULL)
  if (!is.logical(x)) stop('Input must be logical')
  is.na(x) | x
}
