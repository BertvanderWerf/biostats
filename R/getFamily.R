
#' getFamily
#'
#' @param fit a fitted model
#'
#' @return the family of the model
#' @export
#'
#' @examples
#' fit <- glm(count ~ gender, family=binomial, data=data)
#' getFamily(fit)
getFamily <- function(fit) {
  UseMethod('getFamily')
}

#' @export
getFamily.default <- function(fit) {
  return(fit$family$family)
}

#' @export
getFamily.glmerMod <- function(fit) {
  f <- family(fit)
  f$family
}

#' @export
getFamily.coxph <- function(fit) {
  'coxph'
}


