#' getDifferences
#'
#' @param est
#' @param fact
#' @param fact.baseline
#' @param within
#' @param within.baseline
#' @param terms
#' @param estimate
#' @param sep
#' @param use_row_names
#'
#' @return
#' @export
#'
#' @examples
getDifferences <- function(est, fact, fact.baseline=NULL, within=NULL, within.baseline=NULL, terms=c(fact, within), estimate='estimate', sep='.', use_row_names=FALSE) {
  UseMethod("getDifferences")
}

#' @export
getDiffDesignMatrix <- function(data, fact, fact.baseline, within, sep=sep, terms, internal.sep='\10') {
  fact <- interaction(data[,fact], drop=FALSE, sep=internal.sep)
  if (is.null(fact.baseline)) {
    fact.baseline <- levels(fact)[1]
  } else {
    stopifnot(fact.baseline %in% levels(fact))
  }
  dx <- diag(1, nrow=nrow(data))
  if (length(within)>0) {
    within <- interaction(data[,within], drop=FALSE, sep=internal.sep)
    for (i in 1:nlevels(within)) {
      cond1 <- within==levels(within)[i]
      cond2 <- cond1 & (fact==fact.baseline)
      dx[cond1, cond2] <- dx[cond1, cond2] - 1
    }
    if (.row_names_info(data)<0) {
      row.names(dx) <- interaction(data[,terms], drop=FALSE, sep=sep)
    } else {
      row.names(dx) <- row.names(data)
    }
  } else {
    cond2 <- fact==fact.baseline
    dx[,cond2] <- dx[,cond2] - 1
    if (.row_names_info(data)<0) {
      row.names(dx) <- fact
    } else {
      row.names(dx) <- row.names(data)
    }
  }
  colnames(dx) <- row.names(dx)
  if (!(all(rowSums(dx)==0))) {
    stop('multiple fact.baseline values per group, please check the within parameter')
  }
  dx
}

#' @export
getDifferences.default <- function(est, fact, fact.baseline=NULL, within=NULL, within.baseline=NULL, terms=c(fact, within), estimate='estimate', sep='.', use_row_names=FALSE) {
  stopifnot(all(c('predictions','vcov') %in% names(est)))
  data <- est$predictions
  vcov <- est$vcov

  stopifnot(all(fact %in% names(data)))
  if (!is.null(within)) {
    stopifnot(all(within %in% names(data)))
  }
  if (!use_row_names) {
    row.names(data) <- NULL
  }

  Adjust_X <- function(DX) { # sets the x[i,i] values to -1 for rowSums(abs(X))==0
    dDX <- diag(DX)
    dDX[rowSums(abs(DX))==0] <- -1
    diag(DX) <- dDX
    DX
  }

  dx <- getDiffDesignMatrix(data, fact=fact, fact.baseline, within=terms[!(terms %in% fact)], terms, sep=sep)

  dvcov <- adjustVcov(dx %*% vcov %*% t(dx))
  v1 <- (dx*(dx==1)) %*% data[,estimate]
  v2 <- (-dx*(dx==-1)) %*% data[,estimate]
  diff <- setZero(dx %*% data[,estimate])
  diff <- cbind(data[,terms, drop=FALSE], v1=v1, v2=v2, estimate=c(diff), variance=c(diag(dvcov)))
  z<-unlist(sapply(as.data.frame(t(Adjust_X(dx))), function(y) row.names(dx)[y==-1], simplify = TRUE))
  row.names(diff) <- paste0("(",row.names(dx),") - (", z ,')')
  diff <- addUpperLower(diff, df = est$df)
  diff <- addPvalue(diff, df=est$df)

  names <- c('v1','v2')
  if (est$link!='') {
    names(diff) <- replacevalues(names(diff), names, paste(est$link, names, sep='.'))
  }

  res <- list(differences=diff, diff.vcov=dvcov, X=dx)

#   names <- c('v1','v2',estimate','variance','lower.95','upper.95')
#   if (est$link!='') {
#     names(pred) <- replacevalues(names(pred), names, paste(est$link, names, sep='.'))
#   }

  # if needed calculate differences in differences
  if (!is.null(within.baseline)) {
    data <- diff
    vcov <- dvcov
    dx <- getDiffDesignMatrix(data, fact=within, within.baseline, within=terms[!terms %in% within], terms, sep=sep)


    dvcov <- adjustVcov(dx %*% vcov %*% t(dx))
    v1 <- (dx*(dx==1)) %*% data[,'estimate']
    v2 <- (-dx*(dx==-1)) %*% data[,'estimate']
    diff <- setZero(dx %*% data[,'estimate'])
    diff <- cbind(data[,terms, drop=FALSE], v1=v1, v2=v2, estimate=c(diff), variance=c(diag(dvcov)))
    z<-unlist(sapply(as.data.frame(t(Adjust_X(dx))), function(y) row.names(dx)[y==-1], simplify = TRUE))
    row.names(diff) <- paste0("(",row.names(dx),") - (", z ,')')
    diff <- addUpperLower(diff, df = est$df)
    diff <- addPvalue(diff, df=est$df)

    names <- c('v1','v2')
    if (est$link!='') {
      names(diff) <- replacevalues(names(diff), names, paste(est$link, names, sep='.'))
    }

    res$DID <- diff
    res$DID.vcov <- dvcov
    res$X2 <- dx
  }

  if (est$link=='logit') {
    ln.inv.logit <- function(x) { log(exp(x)/(1+exp(x))) }

    res$differences$OR <- exp(res$differences[,'estimate'])
    res$differences$OR.lower.95 <- exp(res$differences[,'estimate']-1.96*sqrt(res$differences[,'variance']))
    res$differences$OR.upper.95 <- exp(res$differences[,'estimate']+1.96*sqrt(res$differences[,'variance']))

    res$differences$RR <- exp(res$X %*% ln.inv.logit(est$predictions[,estimate]))

    if (!is.null(res$DID)) {
      res$DID$OR <- exp(res$DID[,'estimate'])
      res$DID$OR.lower.95 <- exp(res$DID[,'lower.95'])
      res$DID$OR.upper.95 <- exp(res$DID[,'upper.95'])

      res$DID$RR <- exp(res$X2 %*% log(res$differences[,'RR']))
    }
  } else if (est$link=='log') {
    res$differences$RR <- exp(res$differences[,'estimate'])
    res$differences$RR.lower.95 <- exp(res$differences[,'estimate']-1.96*sqrt(res$differences[,'variance']))
    res$differences$RR.upper.95 <- exp(res$differences[,'estimate']+1.96*sqrt(res$differences[,'variance']))

    if (!is.null(res$DID)) {
      res$DID$RR <- exp(res$DID[,'estimate'])
      res$DID$RR.lower.95 <- exp(res$DID[,'lower.95'])
      res$DID$RR.upper.95 <- exp(res$DID[,'upper.95'])
    }
  } else if (!is.null(est$link) && !(est$link %in% c('identity',''))) {
    f <- get(est$linkinv)
    name1 <- paste(est$link, 'v1', sep='.')
    name2 <- paste(est$link, 'v2', sep='.')
    if (name1 %in% names(res$differences) & name2 %in% names(res$differences)) {
      res$differences$diff.median <- f(res$differences[,name1]) - f(res$differences[,name2])
      res$differences$ratio.median <- f(res$differences[,name1]) / f(res$differences[,name2])
    }
   # warning('no rules implemented yet for the inverse of the differences for ',est$link)
  }

  return(res)
}

#' @export
getDifferences.glmerModEstimate <- function(est, fact, fact.baseline=NULL, within=NULL, within.baseline=NULL, terms=c(fact, within), estimate='estimate', sep='.', use_row_names=FALSE) {
  diff <- getDifferences.glmEstimate(est, fact, fact.baseline, within, within.baseline, terms, estimate, sep, use_row_names)
  diff
}

#' @export
getDifferences.glmEstimate <- function(est, fact, fact.baseline=NULL, within=NULL, within.baseline=NULL, terms=c(fact, within), estimate='estimate', sep='.', use_row_names=FALSE) {
  diff <- getDifferences.default(est, fact, fact.baseline, within, within.baseline, terms, estimate, sep, use_row_names)
  diff
}

#' @export
getDifferences.coxphEstimate <- function(est, fact, fact.baseline=NULL, within=NULL, within.baseline=NULL, terms=c(fact, within), estimate='estimate', sep='.', use_row_names=FALSE) {
  diff <- getDifferences.default(est, fact, fact.baseline, within, within.baseline, terms, estimate, sep, use_row_names)
  diff
}


