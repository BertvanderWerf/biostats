#' lnLikPredictions
#'
#' @param pred
#' @param fit
#' @param fun
#' @param nsimulations
#' @param vcov
#'
#' @return
#' @export
#'
#' @examples
lnLikPredictions <- function(pred, fit, fun, nsimulations=10000, vcov=FALSE) {

  stopifnot(all(!sapply(fit$parinfo$mat, is.null)))
  parinfo <- fit$parinfo
  parinfo <- ParInfo(parinfo$pars, parinfo$min, parinfo$max, parinfo$cond, parinfo$fact, data=pred)

  m <- bdiag(parinfo$mat)
  pars <- unlist(fit$parinfo$pars)
  npars <- length(parinfo$pars)
  values <- unlist(m %*% pars)
  if (vcov==TRUE) {
    vcov.parms <- setZero(m %*% fit$vcov %*% t(m))
    var.values <- diag(vcov.parms)
  } else {
    var.values <- values
    for (i in 1:length(var.values)) {
      var.values[i] <- m[i,,drop=FALSE] %*% fit$vcov %*% t(m[i,,drop=FALSE])
    }
  }
  parms <- cbind(pred, parameter=names(parinfo$pars)[1])
  for (i in 2:npars) {
    parms <- rbind(parms, cbind(pred, parameter=names(parinfo$pars)[i]))
  }
  parms$parameter <- factor(parms$parameter, names(parinfo$pars))
  parms <- cbind(parms, par.value=values, par.var=var.values)

  values <- as.data.frame(matrix(values,         nrow(pred), length(parinfo$pars), dimnames=list(NULL,names(parinfo$pars))))
  var.values <- as.data.frame(matrix(var.values, nrow(pred), length(parinfo$pars), dimnames=list(NULL,paste('var',names(parinfo$pars), sep='.'))))
  pred <- cbind(pred, values, var.values)

  func <- paste0('function(x) {', paste(deparse(body(fun)), collapse=''),'}')
  func <- eval(parse(text=func))
  environment(func) <- new.env()

  for (i in 1:length(parnames)) {
    assign(parnames[i], values[,parnames[i]], envir=environment(func))
  }
  Fitted <- func(pred[,fit$x])
  pred <- cbind(pred, fitted=Fitted)

  m1 <- splitMatrix(tcrossprod(m, MASS::mvrnorm(nsimulations, pars, fit$vcov)),
                    rows=length(parnames), cols=1)

  for (i in 1:length(parnames)) {
    assign(parnames[i], m1[[i]], envir=environment(func))
  }
  rm('m1')
  x <- matrix(pred[,fit$x], nrow=nrow(pred), ncol=nsimulations)
  fitted <- func(x)
  fitted <- as.data.frame(t(fitted))
  quantiles <- as.data.frame(t(sapply(fitted, function(x) quantile(x, c(0.5, 0.025, 0.975), na.rm=TRUE))))

  names(pred)
  pred <- cbind(pred, quantiles)
  names(pred)

  cov <- cov(fitted)
  pred <- addDiffPValues(pred, cov, fit$nobs-fit$npar.estimated, 'fitted')

  if (vcov) {
    res <- list(pred=pred, parms=parms, vcov.parms=vcov.parms)
  } else {
    res <- list(pred=pred, parms=parms)
  }
  res
}
