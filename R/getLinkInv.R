#' getLinkInv
#'
#' @param fit a fitted model
#'
#' @return the name of the link function used in the model
#' @export
#'
#' @examples
#' fit <- glm(count ~ gender, family=poisson, data=data)
#' getLinkInv(fit)
getLinkInv <- function(fit) {
  UseMethod("getLinkInv")
}

#' @export
getLinkInv.default <- function(fit) {
  fit$family$linkinv
}

#' @export
getLinkInv.glmerMod <- function(fit) {
  f <- family(fit)
  f$link
}

#' @export
getLinkInv.coxph <- function(fit) {
  'exp'
}
