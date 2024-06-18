
simulateDiff <- function(data, Pair, estimate, variance, firstdiff, seconddiff=NULL, firstbaseline=NULL, secondbaseline=NULL,
                         vars=NULL, inverse=NULL, dinverse=NULL, ddinverse=NULL, sep='\10', nSimulations=0, simvar=NULL) {
  nSimulations <- pmax(0,nSimulations)
  if (is.null(simvar)) nSimulations <- 0

  if (is.null(inverse)) {
    if (estimate=='log') {
      N <- function(x) exp(x)
      inverse <- 'N'
    } else if (estimate=='logit') {
      p <- function(x) { exp(x)/(1+exp(x))}
      Odds <- function(x) exp(x)
      inverse <- c('p','Odds')
    }
  }
  if (is.null(dinverse)) {
    if (estimate=='log') {
      R <- function(x) exp(x)
      dinverse <- 'R'
    } else if (estimate=='logit') {
      OR <- function(x) exp(x)
      dinverse <- 'OR'
    }
  }
  if (is.null(ddinverse)) {
    if (estimate=='log') {
      RR <- function(x) exp(x)
      ddinverse <- 'RR'
    } else if (estimate=='logit') {
      ORR <- function(x) exp(x)
      ddinverse <- 'ORR'
    }
  }

  z <- create.par.and.cov.from.pair(Pair, estimates=estimate, variance = variance)


  z$estimates[,variance] <- diag(z$vcov)
  vars <- if (is.null(Pair$groupby)) vars else Pair$groupby
  stopifnot(all(vars %in% names(data)))

  stopifnot(firstdiff %in% vars)
  stopifnot(is.factor(z$estimates[,firstdiff]))
  if (is.null(firstbaseline)) {
    firstbaseline <- levels(z$estimates[,firstdiff])[1]
  } else {
    stopifnot(firstbaseline %in% levels(z$estimates[,firstdiff]))
  }

  if (!is.null(seconddiff)) {
    stopifnot(seconddiff %in% vars)
    stopifnot(is.factor(z$estimates[,seconddiff]))
    if (is.null(secondbaseline)) {
      secondbaseline <- levels(z$estimates[,seconddiff])[1]
    } else {
      stopifnot(secondbaseline %in% levels(z$estimates[,seconddiff]))
    }
  }

  sep <- '\10'
  zz <- interaction(data[,vars], sep=sep, drop=FALSE)
  zz <- table(zz)
  zz <- cbind(as.data.frame(t(as.matrix(strsplit(names(zz), sep, fixed=TRUE)))), nObs=c(zz))
  names(zz)[1:length(vars)] <- vars
  for(var in vars) {
    if (is.factor(data[,var])) {
      zz[,var] <- factor(zz[,var], levels(data[,var]))
    }
  }
  estimates <- left_join(z$estimates, zz, by=vars)
  estimates$i <- seq(1, nrow(estimates))

  z1 <- droplevels(estimates[estimates[,firstdiff]==firstbaseline,!(names(estimates) %in% c(estimate, variance, 'nObs'))])
  names(z1)[names(z1)=='i'] <- 'j'
  indices <- left_join(estimates, z1[,names(z1)!=firstdiff], by=vars[vars!=firstdiff])
  indices$nObs_j <- indices$nObs[indices$j]
  indices$firstcond <- (indices[,firstdiff]!=firstbaseline) & (indices$nObs!=0) & (indices$nObs_j!=0)

  if (!is.null(seconddiff)) {
    z2 <- droplevels(estimates[estimates[,seconddiff]==secondbaseline ,!(names(estimates) %in% c(estimate, variance, 'nObs'))])
    names(z2)[names(z2)=='i'] <- 'k'
    indices <- left_join(indices, z2[,names(z2)!=seconddiff], by=vars[vars!=seconddiff])
    indices$nObs_k <- indices$nObs[indices$k]
    indices$secondcond <- indices$firstcond & (indices[,seconddiff]!=secondbaseline) & (indices$nObs_k!=0)
  }


  for (nsim in 0:nSimulations) {
    if (nsim==0 & nSimulations>0) {
      library(MASS)
      sims <- MASS::mvrnorm(nSimulations, z$estimates[,estimate], z$vcov)
      Res <- list()
      for (i in 1:length(simvar)) {
        Res[[simvar[i]]] <- sims
      }
    }
    if (nsim>0) {
      indices[,estimate] <- unlist(sims[nsim,])
    }

    if (!is.null(inverse)) {
      for (i in 1:length(inverse)) {
        fun <- get(inverse[i])
        indices[,inverse[i]] <- fun(indices[,estimate])
        indices[,paste0(inverse[i],'.lower.95')] <- fun(indices[,estimate]-1.96*sqrt(indices[,variance]))
        indices[,paste0(inverse[i],'.upper.95')] <- fun(indices[,estimate]+1.96*sqrt(indices[,variance]))
      }
    }

    indices$diff <- indices[,estimate]-indices[indices$j,estimate]
    indices$diff.variance <- NA
    vec <- t(c(1,-1))
    tvec <- t(vec)
    for (k in 1:nrow(indices)) {
      i <- indices$i[k]
      j <- indices$j[k]
      indices$diff.variance[k] <- vec %*% z$vcov[c(i,j), c(i,j)] %*% tvec
    }
    indices$diff.lower.95 <- indices$diff-1.96*sqrt(indices$diff.variance)
    indices$diff.upper.95 <- indices$diff+1.96*sqrt(indices$diff.variance)
    indices$diff.p_value <- (1 - pnorm(abs(indices$diff/sqrt(indices$diff.variance)))) * 2

    indices[,paste0('delta.',estimate)] <- indices[,estimate]-indices[indices$j, estimate]
    indices[,paste0('ratio.',estimate)] <- indices[,estimate]/indices[indices$j, estimate]
    if (!is.null(inverse)) {
      for (i in 1:length(inverse)) {
        fun <- get(inverse[i])
        indices[,paste0('delta.',inverse[i])] <- fun(indices[,estimate])-fun(indices[indices$j, estimate])
        indices[,paste0('ratio.',inverse[i])] <- fun(indices[,estimate])/fun(indices[indices$j, estimate])
      }
    }

    if (!is.null(dinverse)) {
      for (i in 1:length(dinverse)) {
        fun <- get(dinverse[i])
        indices[,dinverse[i]] <- fun(indices$diff)
        indices[,paste0(dinverse[i],'.lower.95')] <- fun(indices$diff-1.96*sqrt(indices$diff.variance))
        indices[,paste0(dinverse[i],'.upper.95')] <- fun(indices$diff+1.96*sqrt(indices$diff.variance))
      }
    }


    # indices$diff <- indices[,estimate]-indices[indices$j,estimate]
    if (!is.null(seconddiff)) {
      indices$diff2 <- indices$diff-indices$diff[indices$k]
      indices$diff2.variance <- NA
      vec <- t(c(1,-1,-1,1))
      tvec <- t(vec)
      for (k in 1:nrow(indices)) {
        i <- indices$i[k]
        j <- indices$j[k]
        i2 <- indices$i[indices$k[k]]
        j2 <- indices$j[indices$k[k]]
        index <- c(i,j,i2,j2)
        indices$diff2.variance[k] <- vec %*% z$vcov[index, index] %*% tvec
      }
    }
    indices$diff2.lower.95 <- indices$diff2 - 1.96*sqrt(indices$diff2.variance)
    indices$diff2.upper.95 <- indices$diff2 + 1.96*sqrt(indices$diff2.variance)
    indices$diff2.p_value <- (1 - pnorm(abs(indices$diff2/sqrt(indices$diff2.variance)))) * 2

    if (!is.null(inverse)) {
      for (i in 1:length(inverse)) {
        indices[,paste0('delta2.',inverse[i])] <- indices[,paste0('delta.',inverse[i])] - indices[indices$k,paste0('delta.',inverse[i])]
        indices[,paste0('ratio2.',inverse[i])] <- indices[,paste0('ratio.',inverse[i])] / indices[indices$k,paste0('ratio.',inverse[i])]
      }
    }

    if (!is.null(ddinverse)) {
      for (i in 1:length(ddinverse)) {
        indices[,ddinverse[i]] <- fun(indices$diff2)
        indices[,paste0(ddinverse[i],'.lower.95')] <- fun(indices$diff2-1.96*sqrt(indices$diff2.variance))
        indices[,paste0(ddinverse[i],'.upper.95')] <- fun(indices$diff2+1.96*sqrt(indices$diff2.variance))
      }
    }
    if (nsim==0) {
      Indices <- indices
    }
    if (nSimulations>0) {
      stopifnot(simvar %in% names(indices))
      if (nsim>0) {
        for (i in 1:length(simvar)) {
          Res[[simvar[i]]][nsim,] <- unlist(indices[,simvar[i]])
        }
      }
    }
  }
  if (nSimulations>0) {
    for (i in 1:length(simvar)) {
      Q <- as.data.frame(t(sapply(as.data.frame(Res[[simvar[i]]]), function(x) quantile(x, c(0.025, 0.5, 0.95), na.rm=TRUE))))
      names(Q) <- replacevalues(names(Q), c('2.5%','50%','95%'), paste(simvar[i],c('lower.95','median','upper.95'), sep='.'))
      row.names(Q) <- NULL
      Indices <- cbind(Indices, Q)
    }
  }
  return(Indices)
}
