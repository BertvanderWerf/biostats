#' getCoefficients
#'
#' @param fit a fitted model
#' @param vcov the covariance of the parameters default NULL uses vcov(fit)
#'
#' @return a dataframe with Estimates, Std. Error, a statistic and P values
#' @export
#'
#' @examples
#' fit <- lm(y ~ AgeGroup, data=data)
#' getCoefficients(fit)
getCoefficients <- function(fit, vcov=NULL) {
  UseMethod('getCoefficients')
}

#' @export
getCoefficients.default <- function(fit, vcov=NULL) {
  out <- as.data.frame(summary(fit)$coefficients)
  if (!is.null(vcov)) {
    res <- data.frame(Estimate=out$Estimate)
    res$'Std. Error' <- sqrt(diag(vcov))
    if ('df' %in% names(out)) {
      res$df <- out$df
    } else {
      res$df <- getDf(fit)
    }
    res$'t value' <- res$Estimate/res$'Std. Error'
    res$'Pr(>|t|)' <- pt(-abs(res$'t value'), res$df)*2
    row.names(res) <- row.names(out)
  } else {
    res <- out
  }
  res$sig <- p_to_stars(res[,grepl('^Pr(*)', names(res), fixed=FALSE)])
  res <- cbind(Parameter=row.names(res), res)
  res
  # res <- cbind(Parameter=row.names(res), res)
  # row.names(res) <- NULL
  # res
}
#'
#' #' @export
#' getCoefficients.coxph <- function(fit) {
#'   res <- as.data.frame(summary(fit)$coefficients)
#'   names(res)[names(res)=='coef'] <- 'Estimate'
#'   res
#' }

#' @export
getCoefficients.lnLik <- function(fit, vcov=NULL) {
  fit$summary
}

#' @export
getCoefficients.gls <- function(fit, vcov=NULL) {
  df <- getDf(fit)
  if (is.null(vcov)) {
    vcov=vcov(fit)
  }
  Values <- coefficients(fit)
  sd <- sqrt(diag(vcov))
  t_values <- Values/sd
  df <- getDf(fit)
  p_values <- setZero(2*pt(-abs(t_values), df))
  res <- data.frame(Estimate=Values, 'Std.Error'=sd,
                    't value'=t_values,
                    'Pr(>|t|)'=p_values, check.names = FALSE)

  res$sig <- p_to_stars(res[,grepl('^Pr(*)', names(res), fixed=FALSE)])
  res
}
