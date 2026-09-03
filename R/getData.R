
#' getData
#'
#' @param fit fitted model
#' @param ...
#'
#' @return the original dataset used in the regression
#' @export
#'
#' @examples
#' fit <- lm(y ~ x, data=data)
#' getData(fit)
getData <- function(fit, ...) {
 UseMethod("getData")
}

#' @export
getData.default <- function(fit, ...) {
  nlme::getData(fit)
}

#' @export
getData.lm <- function(fit, ...) {
  return(fit$model)
}

#' @export
getData.merMod <- function(fit, ...) {
  as.data.frame(fit@frame)
}

#' @export
getData.glmerMod <- function(fit, ...) {
  as.data.frame(fit@frame)
}

# #' @export
# getData.lmerModLmerTest <- function(fit) {
#   as.data.frame(fit@frame)
# }

#' @export
getData.glmmTMB <- function(fit, component=c('cond', 'zi', 'disp'), ...) {
  component <- match.arg(component)
  data <- eval(fit$call$data, envir = environment(formula(fit)))
  # if (component=='zi' && !('zi' %in% names(data))) {
  #   lhs(getFormula(fit, comp=comp))
  # }
  return(data)
}

