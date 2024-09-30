
#' AIC.lnLik
#'
#' @param fit an lnLik structure
#' @param k the penalty parameter k, defaults to 2 influences only AIC
#'
#' @return a named vector containing the values for number of observations (nobs), number of paramters estimated (npar), AIC, AICc and BIC
#' @export
#'
#' @examples
#' AIC(fit) # where fit is an lnLik structure (result of lnLik())
AIC.lnLik <- function(fit, k=2) {
  lnlik <- fit$lnlik
  npar <- fit$npar
  nobs <- fit$nobs
  AIC <- -2*lnlik + k * npar
  AICc <- AIC + (2 * npar*npar+2*npar)/(nobs-npar-1)
  BIC <- AIC + log(nobs) * npar
  c(nobs=nobs,npar=npar,lnlik=lnlik,AIC=AIC, AICc=AICc, BIC=BIC)
}
