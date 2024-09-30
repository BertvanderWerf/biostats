
#' lnLik
#'
#' @description
#' lnLik is used to fit a log likelihood function
#'
#'
#' @param fun The distribution function to be used in the log-likelihood function
#' @param x the name of the vector with the x variables (usually the quantiles)
#' @param parinfo the result of the Parinfo structure with the initial parameter values and relationships
#' @param data the data frame containing the x and possible n values and the variables used in the fact parameter of parinfo
#' @param trace a logical to give trace output at every step
#' @param control a lnLikControl structure
#' @param sep a separator value used in parinfo
#' @param n the name of the vector with the number of observations for each x value
#' @param ... additional variable names to be used in fun
#'
#' @return an lnLik structure containing output
#' @export
#'
#' @examples
lnLik <- function(fun, x, parinfo, data, trace=TRUE, control=lnLikControl(), sep='_', n=NULL, ...) {

  gsolve <- function(a, b=NULL, tol=sqrt(.Machine$double.eps)) {
    ginv <- MASS::ginv(a, tol)
    if (is.null(b)) return(ginv)

    res <- c(ginv %*% b)
    names(res) <- colnames(a)
    res
  }

  result <- list()
  result$control <- control

  if (!(is.character(x) & length(x)==1)) stop('Parameter x must be a variable name of data')
  if (!x %in% names(data)) stop('Parameter x must be a variable name of data')
  result$vars <- x
  result$x <- x
  x <- data[,x]

  if (!is.null(n)) {
    if (!(is.character(x) & length(x)==1)) stop('Parameter x must be a variable name of data')
    if (!x %in% names(data)) stop('Parameter x must be a variable name of data')
    result$vars <- c(result$vars, n)
    result$n <- n
    n <- data[,n]
  }


  args <- names(as.list(match.call()))
  args.extra <- args[!(args %in% names(as.list(match.call(expand.dots=FALSE))))]
  # to do: eval data each arg should be in data

  parinfo <- ParInfo(parinfo$par, parinfo$min, parinfo$max, parinfo$cond, parinfo$fact, data=data, sep=sep)

  npars <- length(parinfo$pars)
  parnames <- names(parinfo$pars)

  result$formula <- parinfo$fact
  result$terms <- result$formula
  result$fixed.vars <- c()
  for (i in 1:npars) {
    if (!is.null(result$terms[[i]])) {
      result$terms[[i]] <- terms(result$terms[[i]])
      result$fixed.vars <- c(result$fixed.vars, labels(result$terms[[i]]))
    }
  }
  result$fixed.vars <- unique(unlist(strsplit(result$fixed.vars, ":")))
  result$fixed.vars <- result$fixed.vars[!(result$fixed.vars %in% c(0,1))]
  result$vars <- c(result$vars, result$fixed.vars)
  result$data <- data[, result$vars]

  parnames.full <- names(unlist(parinfo$pars))
  npars.full <- length(parnames.full)

  pars_cond <- as.logical(unlist(parinfo$cond))
  parnames.selected <- parnames.full[pars_cond]
  npars.selected <- length(parnames.selected)
  par <- unlist(parinfo$pars)[pars_cond]
  par_min <- unlist(parinfo$min)[pars_cond]
  par_max <- unlist(parinfo$max)[pars_cond]


  parinfo$pars <- sapply(1:npars, function(i) { pmin(pmax(parinfo$pars[[i]],parinfo$min[[i]]),parinfo$max[[i]])}, simplify=FALSE)
  names(parinfo$pars) <- parnames

  #environment(fun=fun) <- environment()

  if (!is.null(n) & all(n==1)) {
    n <- NULL
  } else if (!is.null(n)) {
    stopifnot(length(x)==length(n))
  }

  # to do
  if (control$reduce==TRUE) {
    if (!all(pars_cond)) {
      mat <- parinfo$mat
      mat[which(sapply(mat, is.null))] <- matrix(1, 1, 1)
      rcond <- rowSums(as.data.frame(mat)[,pars_cond, drop=FALSE])>0
      if (!all(rcond)) {
        if (!any(rcond)) stop('There is no data for estimation the selected (subset of) parameters')
        result$data <- result$data[rcond,]

        x <- x[rcond]
        if (!is.null(n)) {
          n <- n[rcond]
        }
        for (i in 1:npars) {
          if (!is.null(parinfo$fact)) {
            parinfo$mat[[i]] <- parinfo$mat[[i]][rcond,,drop=FALSE]
          }
        }
        if (length(args.extra)>0) {
          for (i in 1:length(args.extra)) {
            d <- eval(parse(text=args.extra[i]))
            if (is.null(dim(d))) {
              if (length(d)==length(x)) {
                assign(args.extra[i], d[rcond])
                warning('variable ',args.extra[i],' has been reduced due to parameter selection')
              }
            } else if (any(dim(d))==length(x)) {
              if (length(dim(d))==1) {
                assign(args.extra[i], d[rcond])
                warning('variable ',args.extra[i],' has been reduced due to parameter selection')
              } else if (length(dim(d)==2)) {
                j <- match(length(x), dim(d))
                if (j==1) {
                  assign(args.extra[i], d[rcond,])
                } else {
                  assign(args.extra[i], d[,rcond])
                }
              }
            }
          }
        }
      }
    }
  }
  if (is.null(n)) {
    result$nobs <- length(x)
  } else {
    result$nobs <- sum(n)
  }

  if (is.null(n)) {
    ln.lik_i <- paste0('function(x) {log(', paste(deparse(Deriv::Deriv(body(fun), 'x', nderiv=0, cache.exp = FALSE)), collapse=''),')}')
  } else {
    ln.lik_i <- paste0('function(x) { n * log(', paste(deparse(Deriv::Deriv(body(fun), 'x', nderiv=0, cache.exp = FALSE)), collapse=''),')}')
  }

  #if (TRUE) {
    for (i in 1:npars) {
      if (parinfo$link[[i]]!='identity') {
        ln.lik_i <- gsub(paste0("\\b",parnames[i],"\\b"), paste0(link[[i]],"(",parnames[i],")"), ln.lik_i)
      }
    }
  #}

  ln.lik_i <- eval(parse(text=ln.lik_i))

  cond <- sapply(parinfo$cond, sum)>0
  nderiv <- 0:1
  if (control$hessian=='calculate') nderiv<- c(nderiv, 2)
  derivs <- Deriv::Deriv(f=ln.lik_i, x=parnames[cond], cache.exp = TRUE, nderiv=c(0,1,2), combine='cbind')

  # create a new environment for the function derivs. the parameters has to be put inside this environment
  # parameternames can therefore be the same as the variable names used within the function (except for n and x)
  environment(derivs) <- new.env()

  # expand pars to vectors again and put them in environment of function derivs
  #pars <- as.data.frame(mapply(function(x,y) {y %*% t(t(x))}, x=parinfo$pars, y=parinfo$mat)) # only if not any is.null(parinfo$mat)
  pars <- parinfo$pars
  for (i in 1:npars) {
    if (!is.null(parinfo$mat[[i]])) {
      pars[[i]] <- c(pars[[i]] %*% t(parinfo$mat[[i]]))
    }
    assign(parnames[i], pars[[i]], envir=environment(derivs))
  }

  d <- derivs(x)

  if (control$replaceNAN==TRUE) {
    for (i in 1:length(d)) {
      d[[i]][!is.finite(d[[i]])] <- 0
    }
  }


  loglik <- sum(d$`0`)

  GetGradient <- function() {
    if (is.null(dim(d$`1`))) {
      dim(d$`1`) <- c(length(d$`1`), 1)
    }

    k<-0
    J <- NULL
    for (j in 1:npars) {
      if (cond[j]) {
        k <- k+1
        if (!(is.null(parinfo$mat[[j]]))) {
          J <- cbind(J, parinfo$mat[[j]][,parinfo$cond[[j]]==TRUE, drop=FALSE] * d$`1`[,k])
        } else {
          J <- cbind(J, d$`1`[,k, drop=FALSE])
        }
      }
    }
    colnames(J) <- parnames.selected
    J
  }
  d$`1` <- GetGradient()
  J <- colSums(d$`1`, na.rm=TRUE)


  GetHessian <- function() {
    if (control$hessian=='calculate') {
      if (is.null(dim(d$`2`))) {
        dim(d$`2`) <- c(length(d$`2`),1)
      }

      for (j in 1:npars) {
        if (j==1) {
          k <- 0
          H <- matrix(list(), nrow=sum(cond), ncol=sum(cond), dimnames=list(parnames[cond], parnames[cond]))
        }
        for (i in 1:npars) {
          if (cond[i] & cond[j]) {
            #if (i!=j) stop('test')

            k <- k+1
            if (is.null(parinfo$mat[[i]]) & is.null(parinfo$mat[[j]])) {
              H[[k]] <- sum(d$`2`[,k, drop=FALSE])
              dim(H[[k]]) <- c(1,1)
            } else if (is.null(parinfo$mat[[i]])) {
              H[[k]] <- t(colSums(d$`2`[,k]*parinfo$mat[[j]][,parinfo$cond[[j]]==TRUE, drop=FALSE]))
            } else if (is.null(parinfo$mat[[j]])) {
              H[[k]] <- t(t(colSums(d$`2`[,k]*parinfo$mat[[i]][,parinfo$cond[[i]]==TRUE, drop=FALSE])))
            } else {
              h <- matrix(NA, nrow=sum(parinfo$cond[[i]]), ncol=sum(parinfo$cond[[j]]))
              kk <- 0
              for (jj in 1:ncol(parinfo$mat[[j]])) {
                if (parinfo$cond[[j]][jj]==TRUE) {
                  for (ii in 1:ncol(parinfo$mat[[i]])) {
                    if (parinfo$cond[[i]][ii]==TRUE) {
                      kk <- kk+1
                      h[[kk]] <- sum(d$`2`[,k]*parinfo$mat[[i]][,ii]*parinfo$mat[[j]][,jj])
                    }
                  }
                }
              }
              H[[k]] <- h
            }
          }
        }
      }
      #sapply(H, dim, simplify=FALSE)
      hh <- NULL
      k <- 0
      for (i in 1:nrow(H)) {
        c <- NULL
        for (j in 1:ncol(H)) {
          k <- k+1
          c <- rbind(c, H[[k]])
        }
        hh <- cbind(hh, c)
      }
      row.names(hh) <- colnames(hh) <- parnames.selected
      H <- (hh+t(hh))/2

    } else {
      H <- crossprod(d$`1`)
    }
    if (control$method=='Levenberg') {
      H <- H + control$lambda*diag(nrow=npar)
    } else if (control$method=='Marquardt') {
      H <- H + control$lambda*diag(H)
    }
    H
  }
  H <- GetHessian()

  crit <- Inf
  step <- 0
  while (crit>control$eps & step<control$nsteps) {
    step <- step + 1

    if (control$useGInv==TRUE) {
      delta <- gsolve(H, J)
    } else {
      delta <- solve(H, J)
    }
    stopifnot(all(is.finite(delta)))
    par.old <- par
    par <- par - delta

    par <- pmax(pmin(par, par_max), par_min)
    delta <- par - par.old

    # substitute new parameter estimates back in parinfo using parinfo$cond
    for (i in 1:npars) {
      if (i==1) k <- 0
      for (j in 1:length(parinfo$cond[[i]])) {
        if (parinfo$cond[[i]][j]==1) {
          k <- k+1
          parinfo$pars[[i]][j] <- par[k]
        }
      }
    }

    # expand pars to vectors again and put them in environment of function derivs
    pars <- parinfo$pars
    for (i in 1:npars) {
      if (!is.null(parinfo$mat[[i]])) {
        pars[[i]] <- c(pars[[i]] %*% t(parinfo$mat[[i]]))
      }
      assign(parnames[i], pars[[i]], envir=environment(derivs))
    }

    d <- derivs(x)

    if (control$replaceNAN==TRUE) {
      for (i in 1:length(d)) {
        d[[i]][!is.finite(d[[i]])] <- 0
      }
    }

    loglik <- sum(d$`0`)
    d$`1` <- GetGradient()
    J <- colSums(d$`1`, na.rm=TRUE)
    H <- GetHessian()

    crit.old <- crit
    crit <- Inf
    if ('delta' %in% control$criterium) {
      crit <- pmin(sqrt(sum(delta^2)), crit)
    }
    if ('jacobian' %in% control$criterium) {
      crit <- pmin(sqrt(sum(J^2)), crit)
    }
    if ('lnlik' %in% control$criterium) {
      if (step>1) {
        crit <- pmin(abs(loglik.old-loglik), crit)
      }
      loglik.old <- loglik
    }

    if (trace==TRUE) {
      cat.par <- par
      cond.boundary <- par==par_min | par==par_max
      if (any(cond.boundary)) {
        cat.par[cond.boundary] <- paste0('\033[31m',cat.par[cond.boundary],'\033[0m')
      }
      if (step==1) {
        trace.data <- t(c(step,crit, par, J, loglik))
        colnames(trace.data) <- c('step','crit',names(par),paste('gradient',names(par), sep='.'),'loglik')
        cat(colnames(trace.data))
        cat('\n')
      } else {
        trace.data <- rbind(trace.data, c(step,crit, par, J, loglik))
      }
      cat(step,crit, cat.par, J, loglik,'\n')
    }

    if (control$method!='NewtonRaphson'){
      if (crit>crit.old) {
        control$lambda <- control$lambda*control$lambda.multiplier
      } else {
        control$lambda <- control$lambda/control$lambda.multiplier
        par <- par.old
      }
    }
  }

  result$converged <- TRUE
  if (crit>control$eps & step==control$nsteps) {
    cat('No convergence for this criterium within ', control$nsteps, ' steps\n')
    # pars[cond] <- par
    # return(pars)
    result$converged <- FALSE
  }
  cov <- matrix(0, nrow=npars.full, ncol=npars.full, dimnames=list(parnames.full, parnames.full))
  if (control$useGInv==TRUE) {
    cov[pars_cond, pars_cond] <- -gsolve(H)
  } else {
    cov[pars_cond, pars_cond] <- -solve(H)
  }
  var <- diag(cov)


  result$parameters <- as.data.frame(pars)
  pars <- unlist(parinfo$pars)

  parname <- factor(unlist(sapply(1:npars, function(i) { rep(names(parinfo$pars)[i], length(parinfo$pars[[i]])) })),
                    names(parinfo$pars))
  res <- data.frame(parname=parname, estimate=pars,variance=var, is.constant=!pars_cond,
                    min=unlist(parinfo$min), max=unlist(parinfo$max))
  res <- biostats::addUpperLower(res, df=result$nobs-sum(pars_cond))
  res <- biostats::addPvalue(res, df=result$nobs-sum(pars_cond))

  class(result) <- c('lnLik', class(result))
  result$lnlik <- loglik

  result$npar.estimated <- npars.selected
  result$trace <- trace.data
  result$vcov <- cov
  result$summary <- res
  result$coefficients <- unlist(parinfo$pars)
  if (is.null(n)) {
    result$fitted.values <- exp(d$'0')
  } else {
    result$fitted.values <- exp(d$`0`)/n
  }
  result$fun <- fun
  result$parinfo <- parinfo
  result
}

#' @export
summary.lnLik <- function(object) {
  object$summary
}

#' @export
print.lnLik <- function(fit) {
  list(parameters=fit$parinfo$pars,
       submodels=fit$parinfo$fact,
       summary=AIC(fit))
}
