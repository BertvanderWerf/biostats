#' %nin%
#'
#' @description
#' Negation of %in% operator
#'
#' This operator checks if elements are not in a given vector.
#'
#' @export
#' @examples
#' 1 %nin% c(2, 3, 4) # TRUE
`%nin%` <- Negate(`%in%`)
