
#' getTerms
#'
#' @param fit a fitted model
#'
#' @return a terms object
#' @export
#'
#' @examples
#' fit <- lm(y ~ AgeGroup*Gender, data=data)
#' getTerms(fit)
getTerms <- function(fit, ...) {
  UseMethod("getTerms")
}

#' @export
getTerms.default <- function(fit, ...) {
  stats::terms(fit)
}

#' @export
getTerms.glmmTMB <- function(fit, component=c('cond', 'zi', 'disp', 'comb'), ...) {
  component <- match.arg(component)
  stats::terms(getFormula(fit, component))
}
