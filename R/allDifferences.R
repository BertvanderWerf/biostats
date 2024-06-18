#' allDifferences
#'
#' @param estimates a data frame with one of the columns containing the estimates
#' @param vcov the covariance matrix of the estimates
#' @param df the residual degrees of freedom of the model
#' @param estimate the name of the estimate in the data.frame estimates
#'
#' @return a list with the matrix of pairwise differences, matrix with variance of differences (not the covariance) and the matrix of the P-values
#' @export
#'
#' @examples
#' fit <- lm(y ~ agegroup*gender, data = data)
#' pred <- createPredictors(fit, marginal='yes')
#' est <- predictModel(fit, predictors=pred)
#' allDifferences(est$estimates, est$vcov, est$df)
allDifferences <- function(estimates, vcov, df=NULL, estimate='estimate') {
  stopifnot(estimate %in% names(estimates))

  val <- c(estimates[,estimate])
  diff <- matrix(outer(val, val,'-'), nrow=nrow(estimates), ncol=nrow(estimates), dimnames=list(row.names(estimates), row.names(estimates)))
  var.diff <- vcov
  for (i in 1:nrow(vcov)) {
    for(j in 1:ncol(vcov)) {
      var.diff[i,j] <- vcov[i,i]+vcov[j,j]-vcov[i,j]-vcov[j,i]
    }
  }
  z <- diff/sqrt(var.diff)
  if (is.null(df)) {
    P <- (1 - pnorm(abs(z))) * 2
  } else {
    P <- (1 - pt(abs(z), df)) * 2
  }
  diag(P) <- 1
  list(difference=diff, variance=var.diff, P_Values=P)
}
