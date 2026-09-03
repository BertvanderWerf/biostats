#' getDf
#'
#' @param fit a fitted model
#' @param ...
#'
#' @return the residual degrees of freedom for that model
#' @export
#'
#' @examples
#' fit <- lm(y ~ gender, data = data)
#' getDf(fit)
getDf <- function(fit, ...) {
  UseMethod("getDf")
}

#' @export
getDf.default <- function(fit, ...) {
  # fit$df.residual
  nobs(fit) - stats::extractAIC(fit)[1]
}

#' @export
getDf.glmerMod <- function(fit, ...) {
  nobs(fit) - stats::extractAIC(fit)[1]
}

#' @export
getDf.coxph <- function(fit, ...) {
  nobs(fit) - stats::extractAIC(fit)[1]
}

#' @export
getDf.nls <- function(fit, ...) {
  df <- summary(fit)$df
  df[2]
}

#' @export
getDf.gnls <- function(fit, ...) {
  df <- summary(fit)$dims
  df$N - df$p
}

#' @export
getDf.glmmTMB <- function(fit, component=c('cond', 'zi', 'disp'), ...) {
  component <- match.arg(component)
  coef <- getCoefficients(fit, component=component)
  if (!is.null(coef)) {
    s <- summary(fit)
    df <- s$nobs -sum(!is.na(coef$Estimate))
  } else {
    df <- NULL
  }
  return(df)
}
