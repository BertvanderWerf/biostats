#' predictModel
#'
#' @param fit a fitted model
#' @param predictors newData for which predictions and variances are needed
#' @param weights a numeric vector with weights for each record in the predictors
#' @param df the number of residual degrees of freedom, used for calculating the 95% confidence limits. If omitted the function getDf(fit) will be used.
#'
#' @return a list with predictions, covariance matrix of the predictions, family, link and df
#' @export
#'
#' @examples
#' fit <- lm( y ~ ageclass*gender, data=data)
#' pred <- createPredictors(fit)
#' estimates <- predictmodel(fit, predictors=pred)
predictModel <- function(fit, predictors, weights=NULL, df=NULL) {

  formula <- extractFixedFormula(formula(fit))
  names <- getFixedVars(fit)
  c <- getCoefficients(fit)
  x <- model.matrix(formula, data=predictors)
  stopifnot(all(colnames(x)==row.names(c)))

  cond <- (row.names(predictors) %in% row.names(x))
  preds <- x %*% as.matrix(c$Estimates)

  vcov <- as.matrix(vcov(fit))

  pred <- predictors[cond,,drop=FALSE]
  pred$estimate <- preds

  vcov.pred <- x %*% vcov %*% t(x)
  vcov.pred <- (vcov.pred+t(vcov.pred))/2
  pred$variance <- diag(vcov.pred)  # [cond]?

  if (is.null(df)) {
    df <- getDf(fit)
  }

  if (is.null(df)) {
    pred$lower.95 <- pred$estimate - qnorm(0.975) * sqrt(pred$variance)
    pred$upper.95 <- pred$estimate + qnorm(0.975) * sqrt(pred$variance)
  } else {
    pred$lower.95 <- pred$estimate - qt(0.975, df) * sqrt(pred$variance)
    pred$upper.95 <- pred$estimate + qt(0.975, df) * sqrt(pred$variance)
  }
  L <- list(estimates=pred, vcov=vcov.pred, groupby=groupby, df=df)

  L$link <- getLink(fit)
  L$family <- getFamily(fit)

  return(L)
}
