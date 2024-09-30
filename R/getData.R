
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
  if ('data' %in% names(fit)) {
    return(fit$data)
  }
  stop("No 'data' variable can be found in the fitted list for class(",class(fit),"), please add data to the fitted object (see class glm)")
}

#' @export
getData.lm <- function(fit) {
  return(fit$model)
}

#' @export
getData.glmerMod <- function(fit) {
  fit@frame
}
