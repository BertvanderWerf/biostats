#' getModel
#'
#' @param fit
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
getModel <- function(fit, ...) {
  UseMethod("getModel")
}

#' @export
getModel.default <- function(fit, which=c('VeallZimmermann','AIC','BIC','G2','logLik','logLik0')) {
  if ("glm" %in% class(fit)) {
    m <- rbind(formula=condenseFormula(getformula(fit)), as.data.frame(DescTools::PseudoR2(fit, which)))
  } else {
    m <- as.data.frame(c(formula=condenseFormula(getformula(fit)), AIC=AIC(fit), BIC=BIC(fit)))
  }
  m <- cbind(row.names(m), m)
  names(m) <- c('Statistic','Value')
  row.names(m) <- NULL
  return(m)
}

#' @export
getModel.gnls <- function(fit) {
  model <- data.frame(statistic=c('call','formula'), value=c('gnls',deparse(getFormula(fit))))
  model <- rbind(model,
                 data.frame(statistic=names(start), value=sapply(attr(getFormula(fit), 'params'), deparse)))
  model <- rbind(model, data.frame(statistic='AIC', value=extractAIC(fit)[2]))
  model <- rbind(model, data.frame(statistic='npar', value=nrow(coef)))
  model <- rbind(model, data.frame(statistic='n', value=nrow(nlme::getData(fit))))
  model
}

#' @export
getModel.nls <- function(fit) {
  model <- data.frame(statistic=c('call','formula'), value = c('nls',deparse(biostats:::getFormula.default(fit))))
  model <- rbind(model, data.frame(statistic='AIC', value=AIC(fit)))
  df <- summary(fit)$df
  model <- rbind(model, data.frame(statistic='npar', value=df[1]))
  model <- rbind(model, data.frame(statistic='n', value=df[2]+df[1]))
  model
}
