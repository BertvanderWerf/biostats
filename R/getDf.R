#' getDf
#'
#' @param fit a fitted model
#'
#' @return the residual degrees of freedom for that model
#' @export
#'
#' @examples
#' fit <- lm(y ~ gender, data = data)
#' getDf(fit)
getDf <- function(fit) {
  UseMethod("getDf")
}

#' @export
getDf.default <- function(fit) {
  fit$df.residual
}

#' @export
getDf.glmerMod <- function(fit) {
  nobs(fit) - stats::extractAIC(fit)[1]
}
