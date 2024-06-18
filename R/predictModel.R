#' predictModel
#'
#' @param fit a fitted model
#' @param predictors newData for which predictions and variances are needed
#' @param groupby a character vector with the names of the variables of predictors to group the results by
#' @param weights a numeric vector with weights for each record in the predictors
#' @param vcov.out logical value, if true the vcov of the estimates is added as output
#' @param estimate the name of the variable in getcoefficients(fit) denoting the coefficients
#'
#' @return a list with the grouped predictions, variances and 95% Confidence limits, the covariance matrix, predictor names, df, family, link and design matrix X
#' @export
#'
#' @examples
#' fit <- lm( y ~ ageclass*gender, data=data)
#' pred <- createPredictors(fit)
#' predictions <- predictmodel(fit, predictors=pred)
#'
predictModel <- function(fit, predictors, groupby=NULL, weights=NULL, vcov.out=!is.null(groupby), estimate='Estimate') {
  UseMethod("predictModel")
}

#' @export
predictModel.default <- function(fit, predictors, groupby=NULL, weights=NULL, vcov.out=!is.null(groupby), estimate='Estimate') {

  formula <- extractFixedFormula(formula(fit))
  c <- biostats::getCoefficients(fit)
  x <- model.matrix(formula, data=predictors)

  vcov <- as.matrix(vcov(fit))

  if (!is.null(groupby)) {
    pred <- ctable(predictors, names(predictors)[1], rows=groupby, fun=c('nObs'='count'))
    i <- 1
    for (i in 1:nrow(pred)) {
      cond <- rep(TRUE, nrow(predictors))
      for (j in 1:length(groupby)) {
        cond <- cond & (predictors[,groupby[j]]==pred[i,groupby[j]])
      }
      x_i <- colSums(x[cond,, drop=FALSE])
      if (i==1) {
        X <- x_i/sum(cond) # or /sum(weights[cond]) ???
      } else {
        X <- rbind(X, x_i/sum(cond))
      }
    }
    x <- X
  }
  i <- match( row.names(c), colnames(x),nomatch = 0)
  if (any(i==0)) {
    stop('the vector for parameter ',row.names(c)[i==0],' is not found in the design matrix')
  }
  X <- x[,i, drop=FALSE]
  row.names(X) <- NULL
  stopifnot(all(row.names(c)==colnames(X)))

  pred$estimate <- c(X %*% as.matrix(c[,estimate]))

  i <- match(row.names(c), colnames(vcov), nomatch = 0)
  stopifnot(all(i!=0))

  if (vcov.out) {
    vcov.pred <- X %*% vcov[i,i] %*% t(X)
    vcov.pred <- setZero((vcov.pred+t(vcov.pred))/2)
    pred$variance <- diag(vcov.pred)  # [cond]?
  } else {
    pred$variance <- setZero(c(sapply(as.data.frame(t(X)), function(y) t(y) %*% vcov[i,i] %*% y)))
  }

  pred <- addUpperLower(pred, getDf(fit))

  names <- c('estimate','variance','lower.95','upper.95')
  if (getLink(fit)!='') {
    names(pred) <- replacevalues(names(pred), names, paste(getLink(fit), names, sep='.'))
  }

  L <- list()
  L$predictions <-pred
  if (vcov.out) {
    L$vcov <- vcov.pred
  }
  L$X <- x
  L$link <- getLink(fit)
  L$linkinv <- getLinkInv(fit)
  L$family <- getFamily(fit)
  L$df <- getDf(fit)
  L$fixed.vars <- getFixedVars(fit)
  L$groupby <- groupby

  L$estimate <- paste(getLink(fit),'estimate', sep='.')
  L$variance <- paste(getLink(fit),'variance', sep='.')

  return(L)
}

#' @export
predictModel.coxph <- function(fit, predictors, groupby=NULL, weights=NULL, vcov.out=!is.null(groupby), estimate='Coef') {
  est <- predictModel.default(fit, predictors, groupby=groupby, weights=weights, vcov.out=vcov.out, estimate=estimate)

  est$predictions <- addPvalue(est$predictions, est$df, estimate=est$estimate, variance=est$variance)

  est$predictions$H <- exp(est$predictions$log.estimate)
  est$predictions$H.lower.95 <- exp(est$predictions$log.lower.95)
  est$predictions$H.upper.95 <- exp(est$predictions$log.upper.95)

  class(est) <- c('coxphEstimate', class(est))
  est
}

#' @export
predictModel.glm <- function(fit, predictors, groupby=NULL, weights=NULL, vcov.out=!is.null(groupby), estimate='Estimate') {
  est <- predictModel.default(fit, predictors, groupby=groupby, weights=weights, vcov.out=vcov.out, estimate=estimate)

  est$predictions <- addPvalue(est$predictions, est$df, estimate=est$estimate, variance=est$variance)

  if (est$family=='binomial' & est$link=='logit') {
    est$predictions$Odds <- exp(est$predictions[,'logit.estimate'])
    est$predictions$Odds.lower.95 <- exp(est$predictions[,'logit.lower.95'])
    est$predictions$Odds.upper.95 <- exp(est$predictions[,'logit.upper.95'])

    inv.logit <- function(x) exp(x)/(1+exp(x))
    est$predictions$p <- inv.logit(est$predictions[,'logit.estimate'])
    est$predictions$p.lower.95 <- inv.logit(est$predictions[,'logit.lower.95'])
    est$predictions$p.upper.95 <- inv.logit(est$predictions[,'logit.upper.95'])
  } else if (est$family=='poisson' & est$link=='log') {
    est$predictions$estimate <- exp(est$predictions[,'log.estimate'])
    est$predictions$lower.95 <- exp(est$predictions[,'log.lower.95'])
    est$predictions$upper.95 <- exp(est$predictions[,'log.upper.95'])
  }

  class(est) <- c('glmEstimate', class(est))
  est
}
