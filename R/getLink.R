#' getLink
#'
#' @param fit a fitted model
#'
#' @return the name of the link function used in the model
#' @export
#'
#' @examples
#' fit <- glm(count ~ gender, family=poisson, data=data)
#' getLink(fit)
getLink <- function(fit) {
  UseMethod("getLink")
}

#' @export
getLink.default <- function(fit) {
  fit$family$link
}

#' @export
getLink.glmerMod <- function(fit) {
  f <- family(fit)
  f$link
}

#' @export
getLink.coxph <- function(fit) {
  'log'
}
