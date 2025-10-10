#' predictModel
#'
#' @param fit a fitted model
#' @param predictors newData for which predictions and variances are needed
#' @param groupby a character vector with the names of the variables of predictors to group the results by
#' @param weights a numeric vector with weights for each record in the predictors, weights are ignored when groupby=NULL
#' @param vcov a predefined variance-covariance to be used instead of the automatically calculated stats::vcov(fit) when vcov.out=TRUE, vcov.out will be set to TRUE
#' @param vcov.out logical value, if true the vcov of the estimates is added as output
#' @param estimate the name of the variable in getcoefficients(fit) denoting the coefficient values
#' @param ... additional parameters
#'
#' @return a list with the grouped predictions, variances and 95% Confidence limits, the covariance matrix, predictor names, df, family, link and design matrix X
#' @export
#'
#' @examples
#' fit <- lm( y ~ ageclass*gender, data=data)
#' pred <- createPredictors(fit)
#' predictions <- predictmodel(fit, predictors=pred)
#'
predictModel <- function(fit, predictors, groupby=NULL, weights=NULL, vcov=NULL, vcov.out=TRUE, estimate='Estimate', ...) {
  UseMethod("predictModel")
}

#' @export
predictModel.default <- function(fit, predictors, groupby=NULL, weights=NULL, vcov=NULL, vcov.out=TRUE, estimate='Estimate') {

  if (!is.null(groupby) && !is.null(weights)) { # weights is only used if groupby is not null
    stopifnot(weights %in% names(predictors))
    stopifnot(weights %nin% groupby)
  }

  formula <- extractFixedFormula(formula(fit))

  # check if all possible levels of factors (or text) from predictors are in the data used to create the model
  # set extra levels to missing, give warning and remove records from predictors
  vars <- attr(formula, "term.labels")[attr(formula,"order")==1]
  data <- getData(fit)[,vars, drop=FALSE]
  cond_not_na <- rep(TRUE, nrow(predictors))
  for (var in vars) {
    if (class(data[,var])!=class(predictors[,var])) {
      stop("type mismatch for variable ", var, " in getData() and predictors")
    }
    if (is.factor(data[,var])) {
      predictors[,var] <- factor(predictors[,var], levels(data[,var]))
    } else if (is.character(data[,var])) {
      predictors[predictors[,var] %nin% c(unique(data[,var])),var] <- NA
    }
    cond_not_na <- cond_not_na & (!is.na(predictors[,var]))
  }
  if (sum(!cond_not_na)>0) {
    warning('there are missing values in one or more predictor variables, predictors will be reduced by ', sum(!cond_not_na),' records before calculations')
    predictors <- predictors[cond_not_na,, drop=FALSE]
  }

  pred <- predictors # pred will be modified, predictors are needed

  c <- biostats::getCoefficients(fit)
  x <- model.matrix(formula, data=pred)

  if (is.null(vcov)) {
    vcov <- as.matrix(vcov(fit))
  }

  bDebug <- TRUE
  if (!is.null(groupby)) {
    if (is.null(weights)) {
      weights <- 'Weights'
      predictors[,weights] <- 1
      f <- 'sum'
      names(f) <- paste('total',weights,sep='.')
      pred <- as.data.frame(ctable(predictors, weights, rows=groupby, fun=f, hsep=c('$','#')))[, -1, drop=FALSE]
    } else {
      f <- 'sum'
      names(f) <- paste('total',weights,sep='.')
      pred <- as.data.frame(ctable(predictors, weights, rows=groupby, fun=f, hsep=c('$','#')))[, -1, drop=FALSE]
    }
    i <- 1
    for (i in 1:nrow(pred)) {
      cond <- rep(TRUE, nrow(predictors))
      for (j in 1:length(groupby)) {
        cond <- cond & (predictors[,groupby[j]]==pred[i,groupby[j]])
      }
      if (bDebug) {
        v1 <- x[cond,,drop=FALSE] # 799 x 32
        v2 <- predictors[cond,weights]
        x_i <- v2 %*% v1
        #x_i <- colSums(x[cond,, drop=FALSE]) #
        if (i==1) {
          X <- x_i/sum(predictors[cond, weights])
        } else {
          X <- rbind(X, x_i/sum(predictors[cond, weights]))
        }
      } else {
        x_i <- colSums(x[cond,, drop=FALSE]) # colSums(x[cond,,drop=FALSE]*predictors[cond,weights])
        if (i==1) {
          X <- x_i/sum(cond) # or /sum(weights[cond]) ??? sum(predictors[cond, weights])??
        } else {
          X <- rbind(X, x_i/sum(cond))
        }
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
  if (!(getLink(fit) %in% c('','identity'))) {
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

  if (!(getLink(fit) %in% c('','identity'))) {
    L$estimate <- paste(getLink(fit),'estimate', sep='.')
    L$variance <- paste(getLink(fit),'variance', sep='.')
  } else {
    L$estimate <- 'estimate'
    L$variance <- 'variance'
  }

  L$predictions <- addPvalue(L$predictions, L$df, estimate=L$estimate, variance=L$variance)
  if (L$link=='logit') {
    L$predictions$Odds <- exp(L$predictions[,'logit.estimate'])
    L$predictions$Odds.lower.95 <- exp(L$predictions[,'logit.lower.95'])
    L$predictions$Odds.upper.95 <- exp(L$predictions[,'logit.upper.95'])

    inv.logit <- function(x) exp(x)/(1+exp(x))
    L$predictions$p <- inv.logit(L$predictions[,'logit.estimate'])
    L$predictions$p.lower.95 <- inv.logit(L$predictions[,'logit.lower.95'])
    L$predictions$p.upper.95 <- inv.logit(L$predictions[,'logit.upper.95'])
  } else if (L$link=='log') {
    L$predictions$estimate <- exp(L$predictions[,'log.estimate'])
    L$predictions$lower.95 <- exp(L$predictions[,'log.lower.95'])
    L$predictions$upper.95 <- exp(L$predictions[,'log.upper.95'])
  } else if (!is.null(L$link) && !(L$link %in% c('identity',''))) {
    f <- L$linkinv
    if (is.character(f)) f <- get(f)
    L$predictions$estimate <- f(L$predictions[,paste(L$link,'estimate', sep='.')])
    L$predictions$lower.95 <- f(L$predictions[,paste(L$link,'lower.95', sep='.')])
    L$predictions$upper.95 <- f(L$predictions[,paste(L$link,'upper.95', sep='.')])
  }

  return(L)
}

#' @export
predictModel.coxph <- function(fit, predictors, groupby=NULL, weights=NULL, vcov=NULL, vcov.out=TRUE, estimate='Coef') {
  est <- predictModel.default(fit, predictors, groupby=groupby, weights=weights, vcov=vcov, vcov.out=vcov.out, estimate=estimate)

  # est$predictions <- addPvalue(est$predictions, est$df, estimate=est$estimate, variance=est$variance)

  est$predictions$H <- exp(est$predictions$log.estimate)
  est$predictions$H.lower.95 <- exp(est$predictions$log.lower.95)
  est$predictions$H.upper.95 <- exp(est$predictions$log.upper.95)

  class(est) <- c('coxphEstimate', class(est))
  est
}

#' @export
predictModel.glm <- function(fit, predictors, groupby=NULL, weights=NULL, vcov=NULL, vcov.out=TRUE, estimate='Estimate') {
  est <- predictModel.default(fit, predictors, groupby=groupby, weights=weights, vcov=vcov, vcov.out=vcov.out, estimate=estimate)

  # est$predictions <- addPvalue(est$predictions, est$df, estimate=est$estimate, variance=est$variance)
  #
  # if (est$link=='logit') {
  #   est$predictions$Odds <- exp(est$predictions[,'logit.estimate'])
  #   est$predictions$Odds.lower.95 <- exp(est$predictions[,'logit.lower.95'])
  #   est$predictions$Odds.upper.95 <- exp(est$predictions[,'logit.upper.95'])
  #
  #   inv.logit <- function(x) exp(x)/(1+exp(x))
  #   est$predictions$p <- inv.logit(est$predictions[,'logit.estimate'])
  #   est$predictions$p.lower.95 <- inv.logit(est$predictions[,'logit.lower.95'])
  #   est$predictions$p.upper.95 <- inv.logit(est$predictions[,'logit.upper.95'])
  # } else if (est$link=='log') {
  #   est$predictions$estimate <- exp(est$predictions[,'log.estimate'])
  #   est$predictions$lower.95 <- exp(est$predictions[,'log.lower.95'])
  #   est$predictions$upper.95 <- exp(est$predictions[,'log.upper.95'])
  # }

  class(est) <- c('glmEstimate', class(est))
  est
}

#' @export
predictModel.glmerMod <- function(fit, predictors, groupby=NULL, weights=NULL, vcov=NULL, vcov.out=TRUE, estimate='Estimate') {
  est <- biostats:::predictModel.glm(fit, predictors, groupby=groupby, weights=weights, vcov=vcov, vcov.out=vcov.out, estimate=estimate)
  class(est) <- c('glmerModEstimate', class(est)[class(est)!='glmEstimate'])
  est
}

#' @export
predictModel.lmerModLmerTest <- function(fit, predictors, groupby=NULL, weights=NULL, vcov=NULL, vcov.out=TRUE, estimate='Estimate') {
  est <- biostats:::predictModel.glm(fit, predictors, groupby=groupby, weights=weights, vcov=vcov, vcov.out=vcov.out, estimate=estimate)
  class(est) <- c('lmerModLmerTestEstimate', class(est)[class(est)!='glmEstimate'])
  est
}

#' @export
predictModel.lmerMod <- function(fit, predictors, groupby=NULL, weights=NULL, vcov=NULL, vcov.out=RUE, estimate='Estimate') {
  est <- biostats:::predictModel.glm(fit, predictors, groupby=groupby, weights=weights, vcov=vcov, vcov.out=vcov.out, estimate=estimate)
  class(est) <- c('lmerModlmerMod', class(est)[class(est)!='glmEstimate'])
  est
}


#' @export
predictModel.nls <- function(fit, predictors, groupby=NULL, weights=NULL, vcov=NULL, vcov.out=TRUE, estimate='Estimate', number_of_simulatons=1000) {

  cond <- rep(TRUE, nrow(predictors))
  for (i in 1:length(fit$contrasts)) {
    cond <- cond & predictors[,names(fit$contrasts)[i]] %in% row.names(fit$contrasts[[i]])
  }
  if (!all(cond)) {
    warning('there are missing values in one or more predictor variables, predictors will be reduced by ', sum(!cond),' records before calculations')
    predictors <- droplevels(predictors[cond,])
  }

  vars <- all.vars(extractFixedFormula(formula(fit)))
  vars <- as.list(vars, all.names=TRUE)

  # c <- fit$call
  # extracted_function <- get(as.character(c[[1]]))
  # matched_call <- match.call(extracted_function, c)
  # args <- as.list(matched_call)[-1]
  # params <- eval(args$params)

  pars <- fit$m$getAllPars()

  predict <- function(fun, vars, pars) {
    vars <- vars[vars %in% names(data)]
    for (var in vars) {
      eval(parse(text=paste(var, 'predictors[[var]]', sep='=')), envir=environment())
    }
    for (i in 1:length(pars)) {
      eval(parse(text=paste(names(pars)[i], 'pars[[i]]', sep='=')), envir=environment())
    }
    predictors <- eval(parse(text=deparse(rhs(fun))))
    return(predictors)
  }

  fitted <- predict(formula(fit), vars, pars)
  res <- cbind(predictors, fitted)

  if (number_of_simulatons>0) {
    if (is.null(vcov)) {
      vcov <- vcov(fit)
    }
    sim <- MASS::mvrnorm(n=number_of_simulatons, mu=coef(fit), Sigma=vcov)
    simulations_par <- as.data.frame(sim)
    sim <- as.data.frame(t(sim))

    s <- c(sim[1])
    out <- sapply(sim, function(s) { s <- c(s); names(s) <- row.names(sim); predict(formula(fit), vars, s)})
    out <- as.data.frame(t(out))

    out1 <- as.data.frame(t(sapply(out, quantile, probs=c(0.5, 0.025, 0.975), na.rm=TRUE)))
    names(out1) <- c('Median', 'lower.95', 'upper.95')
    out2 <- sapply(out, mean, na.rm=TRUE)
    out3 <- sapply(out, stats::var, na.rm=TRUE)
    res <- cbind(res, out1, mean=out2, var=out3)
  }

  out <- list(predictions=res, simulations_par=sim, simulations_predictions=out)
  if (vcov.out==TRUE & number_of_simulatons>0) {
    out$vcov <- cov(as.matrix(out$simulations_predictions),use="pairwise.complete.obs")
  }

  if (!is.null(groupby)) {
    warning('the option groupby has not been implemented yet')
  }

  out
}

#' @export
predictModel.gnls <- function(fit, predictors, groupby=NULL, weights=NULL, vcov=NULL, vcov.out=TRUE, estimate='Estimate', number_of_simulatons=1000) {

  cond <- rep(TRUE, nrow(predictors))
  if (!is.null(fit$contrasts) && length(fit$contrasts)>0) {
    for (i in 1:length(fit$contrasts)) {
      cond <- cond & predictors[,names(fit$contrasts)[i]] %in% row.names(fit$contrasts[[i]])
    }
    if (!all(cond)) {
      warning('there are missing values in one or more predictor variables, predictors will be reduced by ', sum(!cond),' records before calculations')
      predictors <- droplevels(predictors[cond,])
    }
  }

  vars <- all.vars(extractFixedFormula(formula(fit)))
  vars <- vars[!vars %in% names(fit$plist)]
  #vars <- as.list(vars, all.names=TRUE)

  Params <- getFormula(fit)
  Params <- attr(Params, 'params')

  plist <- lapply(Params, function(f) { model.matrix(as.formula(paste0('~', deparse(rhs(f)))), predictors) })

  pars <- lapply(fit$pmap, function(index) coefficients(fit)[index])
  pars <- mapply(function(x, y) { c(x %*% y) }, plist, pars, SIMPLIFY=FALSE, USE.NAMES = TRUE)

  predict <- function(fun, vars, pars) {
    vars <- vars[vars %in% names(predictors)]
    for (var in vars) {
      eval(parse(text=paste(var, 'predictors[[var]]', sep='=')), envir=environment())
    }
    for (i in 1:length(pars)) {
      eval(parse(text=paste(names(pars)[i], 'pars[[i]]', sep='=')), envir=environment())
    }
    predictors <- eval(parse(text=deparse(rhs(fun))), envi=environment())
    return(predictors)
  }

  fitted <- predict(formula(fit), vars, pars)
  res <- cbind(predictors, fitted)

  if (number_of_simulatons>0) {
    n <- length(plist)
    if (is.null(vcov)) {
      vcov <- vcov(fit)
    }
    sim <- MASS::mvrnorm(n=number_of_simulatons, mu=coef(fit), Sigma=vcov)
    sim <- as.data.frame(t(sim))
    out <-sapply(sim, function(s) {
      pars_i <- lapply(seq_len(n), function(i) { plist[[i]] %*% s[fit$pmap[[i]]] })
      names(pars_i) <- names(plist)
      predict(formula(fit), vars, pars_i)
    })
    out <- as.data.frame(t(out))
    out1 <- as.data.frame(t(sapply(out, quantile, probs=c(0.5, 0.025, 0.975), na.rm=TRUE)))
    names(out1) <- c('Median', 'lower.95', 'upper.95')
    out2 <- sapply(out, mean, na.rm=TRUE)
    out3 <- sapply(out, stats::var, na.rm=TRUE)
    res <- cbind(res, out1, mean=out2, var=out3)
  }
  out <- list(predictions=res, simulations_par=sim, simulations_predictions=out)
  if (vcov.out==TRUE & number_of_simulatons>0) {
    out$vcov <- cov(as.matrix(out$simulations_predictions),use="pairwise.complete.obs")
  }

  if (!is.null(groupby)) {
    warning('the option groupby has not been implemented yet')
    if (FALSE) {
      groupby <- groupby[groupby %in% names(predictors)]
      predictors <- predictors
      if (is.null(weights)) {
        weights <- 'Weights'
        predictors[,weights] <- 1
        f <- 'sum'
        names(f) <- paste('total',weights,sep='.')
        predictors <- as.data.frame(ctable(predictors, weights, rows=groupby, fun=f, hsep=c('$','#')))[, -1, drop=FALSE]
      } else {
        f <- 'sum'
        names(f) <- paste('total',weights,sep='.')
        predictors <- as.data.frame(ctable(predictors, weights, rows=groupby, fun=f, hsep=c('$','#')))[, -1, drop=FALSE]
      }
      for (var in groupby) {
        predictors[,var] <- factor(predictors[,var])
      }
      dim(predictors)
      x <- model.matrix(as.formula(paste('~0+', paste(groupby, collapse=':'))),  predictors)
      dim(x)
      x <- outer(predictors$Weights, c(predictors$Weights %*% x), '/') * x
      dim(x)
      predictors$Estimate <- c(predictors$fitted %*% x)

      vcov.predictors <- x %*% f$vcov %*% t(x)
      dim(x)
      out <- list(predictors=predictors, x=x)
    }
  }

  out
}
