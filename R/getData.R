
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
  nlme::getData(fit)
}

#' @export
getData.lm <- function(fit) {
  return(fit$model)
}

#' @export
getData.merMod <- function(fit) {
  as.data.frame(fit@frame)
}

#' @export
getData.glmerMod <- function(fit) {
  as.data.frame(fit@frame)
}

# #' @export
# getData.lmerModLmerTest <- function(fit) {
#   as.data.frame(fit@frame)
# }


