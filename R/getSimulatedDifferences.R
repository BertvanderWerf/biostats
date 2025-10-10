#' simulated_p_value
#' calculates the p_value of a distribution assumed to have a mean of h0 and an alternative hypothesis wich is usually the mean of the x_values
#'
#' compare for t values e.g. double <- pt(-1.96, 10000) + (1-pt(1.96, 10000)) which is about 0.05
#' and 1.96 is the absolute difference under H0
#' we assume distribution stays the same only mean value changes
#'
#' @param x_simulated raw simulated distribution
#' @param h0 Assumed real mean value of the distribution
#' @param h1 usually observed mean value of x_simulated as alternative hypothesis
#'
#' @returns a dataframe with left_sided, right_sided and two_sided p_values for the difference between h1 and ho (diff) and the values H0 and H1
#' @export
#'
#' @examples
#' p_value_sim(rnorm(10000), h0=1.96, h1=0)
simulated_p_value <- function(x_simulated, h0, h1=mean(x_simulated, na.rm=TRUE)) {
  observed_n <- sum(!is.na(x_simulated))
  diff <- h0 - h1

  left <- sum(x_simulated - h1 +h0 <=h0-abs(diff))/observed_n
  right <- 1- sum(x_simulated - h1+h0 <= h0+abs(diff))/observed_n
  double <- left+right
  data.frame(left_sided=left, right_sided=right, two_sided=double, diff=diff, H0=h0, h1=h1)
}



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
#' @param diff_h0 default 0
#' @param ratio_h0 default 1
#' @param p_value_type default two_dided
#'
#' @return
#' @export
#'
#' @examples
getSimulatedDifferences <- function(est, fact, fact.baseline=NULL, within=NULL, within.baseline=NULL, terms=c(fact, within), estimate='estimate', sep='.', nsimulations=10000,
                                    diff_h0=0, ratio_h0=1, p_value_type='two_sided') {
  getQuantiles <- function(m, name=NULL, alpha=0.05) {
    m <- as.data.frame(t(sapply(as.data.frame(m), function(x) quantile(x, c(alpha/2,0.5,1-alpha/2), na.rm=TRUE))))
    if (!is.null(name)) {
      num <- round(100*(1-alpha),0)
      names(m) <- paste(name,c(paste('lower',num, sep = '.'), 'median',
                               paste('upper',num, sep = '.')), sep='.')
    }
    m
  }

  # P_values <- function(x, val) {
  #   n <- sum(!is.na(x))
  #   right_sided <- sum(x>=val, na.rm=TRUE)/n
  #   left_sided <- sum(x<=val, na.rm=TRUE)/n
  #   two_sided <- pmin(1, pmax(0, 2*pmin(left_sided, right_sided)))
  #   data.frame(left_sided=left_sided, right_sided=right_sided, two_sided=two_sided)
  # }

  stopifnot(all(c('predictions','vcov') %in% names(est)))
  data <- est$predictions
  vcov <- est$vcov

  stopifnot(all(fact %in% names(data)))
  if (!is.null(within)) {
    stopifnot(all(within %in% names(data)))
  }
  row.names(data) <- NULL

  Adjust_X <- function(DX) { # sets the x[i,i] values to -1 for rowSums(abs(X))==0
    dDX <- diag(DX)
    dDX[rowSums(abs(DX))==0] <- -1
    diag(DX) <- dDX
    DX
  }

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

  d_p <- as.data.frame(t(as.matrix(lapply(as.data.frame(d), simulated_p_value, h0=diff_h0))))
  r_p <- as.data.frame(t(as.matrix(lapply(as.data.frame(r), simulated_p_value, h0=ratio_h0))))

  D <- getQuantiles(d, 'diff')
  R <- getQuantiles(r, 'ratio')

  v1 <- sapply(as.data.frame(v1), function(x) { median(x, na.rm=TRUE)})
  v2 <- sapply(as.data.frame(v2), function(x) { median(x, na.rm=TRUE)})

  diff <- cbind(data[,terms, drop=FALSE], v1=v1, v2=v2, D, diff.P_value=d_p[[p_value_type]], R, ratio.P_value=r_p[[p_value_type]])

  z<-unlist(sapply(as.data.frame(t(Adjust_X(dx))), function(y) row.names(dx)[y==-1], simplify = TRUE))
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

    dd_p <- as.data.frame(t(as.matrix(lapply(as.data.frame(dd), simulated_p_value, h0=diff_h0))))
    rr_p <- as.data.frame(t(as.matrix(lapply(as.data.frame(rr), simulated_p_value, h0=ratio_h0))))

    DD <- getQuantiles(dd, 'diff')
    RR <- getQuantiles(rr, 'ratio')


    v1 <- sapply(as.data.frame(d1), function(x) { median(x, na.rm=TRUE)})
    v2 <- sapply(as.data.frame(d2), function(x) { median(x, na.rm=TRUE)})
    r1 <- sapply(as.data.frame(r1), function(x) { median(x, na.rm=TRUE)})
    r2 <- sapply(as.data.frame(r2), function(x) { median(x, na.rm=TRUE)})

    res$DID <- cbind(data[,terms, drop=FALSE], d1=v1,
                     d2=v2, r1=r1, r2=r2, DD, diff.P_value=dd_p[[p_value_type]], RR, ratio.P_value=rr_p[[p_value_type]])

    z<-unlist(sapply(as.data.frame(t(Adjust_X(dx))), function(y) row.names(dx)[y==-1], simplify = TRUE))
    row.names(res$DID) <- paste0("(",row.names(dx),") - (", z ,')')

    res$X2 <- dx
  }
  res
}
