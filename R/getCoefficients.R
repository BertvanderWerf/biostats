#' getCoefficients
#'
#' @param fit a fitted model
#'
#' @return a dataframe with Estimates, Std. Error, a statistic and P values
#' @export
#'
#' @examples
#' fit <- lm(y ~ AgeGroup, data=data)
#' getCoefficients(fit)
getCoefficients <- function(fit) {
  UseMethod('getCoefficients')
}

#' @export
getCoefficients.default <- function(fit) {
  as.data.frame(summary(fit)$coefficients)
}
