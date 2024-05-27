
#' getData
#'
#' @param fit fitted model
#'
#' @return the original dataset used in the regression
#' @export
#'
#' @examples
#' fit <- lm(y ~ x, data=data)
#' getData(fit)
getData <- function(fit) {
  UseMethod("getData")
}

#' @export
getData.default <- function(fit) {
  return(lme4::getData(fit))
}

#' @export
getData.glm <- function(fit) {
  return(fit$data)
}

#' @export
getData.lm <- function(fit) {
  return(fit$model)
}
