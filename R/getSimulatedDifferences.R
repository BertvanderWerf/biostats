#' getSimulatedDifferences
#'
#' @param est
#' @param fact
#' @param fact.baseline
#' @param within
#' @param within.baseline
#' @param terms
#' @param estimate
#' @param sep
#' @param nsimulations
#'
#' @return
#' @export
#'
#' @examples
getSimulatedDifferences <- function(est, fact, fact.baseline=NULL, within=NULL, within.baseline=NULL, terms=c(fact, within), estimate='estimate', sep='.', nsimulations=10000) {
  getQuantiles <- function(m, name=NULL, alpha=0.05) {
    m <- as.data.frame(t(sapply(as.data.frame(m), function(x) quantile(x, c(alpha/2,0.5,1-alpha/2), na.rm=TRUE))))
    if (!is.null(name)) {
      num <- round(100*(1-alpha),0)
      names(m) <- paste(name,c(paste('lower',num, sep = '.'), 'median',
                               paste('upper',num, sep = '.')), sep='.')
    }
    m
  }

  stopifnot(all(c('predictions','vcov') %in% names(est)))
  data <- est$predictions
  vcov <- est$vcov

  stopifnot(all(fact %in% names(data)))
  if (!is.null(within)) {
    stopifnot(all(within %in% names(data)))
  }
  row.names(data) <- NULL

  dx <- getDiffDesignMatrix(data, fact, fact.baseline, terms[!(terms %in% fact)], terms, sep=sep)
  Sims <- MASS::mvrnorm(nsimulations, data[,estimate], vcov)

  if (is.null(est$linkinv)) {
    warning("est$linkinv is null, no inverse function specified")
    vals <- Sims
  } else {
    if (is.function(est$linkinv)) {
      f <- est$linkinv
    } else if (is.character(est$linkinv)) {
      f <- get(est$linkinv)
    } else {
      stop('est$linkinv should be a function or a characterstring')
    }
    vals <- f(Sims)
  }
  v1 <- vals %*% t(dx*(dx>0))
  v2 <- -vals %*% t(dx*(dx<0))
  d <- v1-v2
  r <- ifelse(v1==0 & v2==0, 1, v1/v2)

  D <- getQuantiles(d, 'diff')
  R <- getQuantiles(r, 'ratio')

  v1 <- sapply(as.data.frame(v1), function(x) { median(x, na.rm=TRUE)})
  v2 <- sapply(as.data.frame(v2), function(x) { median(x, na.rm=TRUE)})

  diff <- cbind(data[,terms, drop=FALSE], v1=v1, v2=v2, D, R)

  z<-unlist(sapply(as.data.frame(t(dx)), function(y) row.names(dx)[y==-1], simplify = TRUE))
  row.names(diff) <- paste0("(",row.names(dx),") - (", z ,')')

  res <- list(differences=diff, X=dx, simulations = Sims)

  # if needed calculate differences in differences
  if (!is.null(within.baseline)) {
    data <- diff
    dx <- getDiffDesignMatrix(data, within, within.baseline, terms[!terms %in% within], terms, sep=sep)

    d1 <- d %*% t(dx*(dx>0))
    d2 <- -d %*% t(dx*(dx<0))
    dd <- d1 - d2

    r1 <- r %*% t(dx*(dx>0))
    r2 <- -r %*% t(dx*(dx<0))
    rr <- ifelse(r1==0 & r2==0, 1, r1/r2)

    DD <- getQuantiles(dd, 'diff')
    RR <- getQuantiles(rr, 'ratio')


    v1 <- sapply(as.data.frame(d1), function(x) { median(x, na.rm=TRUE)})
    v2 <- sapply(as.data.frame(d2), function(x) { median(x, na.rm=TRUE)})
    r1 <- sapply(as.data.frame(r1), function(x) { median(x, na.rm=TRUE)})
    r2 <- sapply(as.data.frame(r2), function(x) { median(x, na.rm=TRUE)})

    res$DID <- cbind(data[,terms, drop=FALSE], d1=v1,
                     d2=v2, r1=r1, r2=r2, DD, RR)

    z<-unlist(sapply(as.data.frame(t(dx)), function(y) row.names(dx)[y==-1], simplify = TRUE))
    row.names(res$DID) <- paste0("(",row.names(dx),") - (", z ,')')

    res$X2 <- dx
  }
  res
}
