
#' The Weibull Distribution
#'
#' Density (pdf) and cumulative density (cdf) Weibull distribution functions with
#' parameters shape (k), scale(lambda) and location (c). The functions are well suited
#' to be used in the lnLik function as their derivatives have already been derived
#' pdfWeibull, cdfWeibull are in principle reparamaterisations of the dweibull and
#' qweibull of the stats package. In addition moment estimaters can be made using the
#' function initWeibull. Summary statistics can be derived from the parameters using
#' meanWeibull, medianWeibull, modeWeibull. The hazard function hazardWeibull has
#' been added too.
#'
#' @param x vector of quantiles
#' @param k shape paramter
#' @param lambda scale paramter
#' @param c offset parameter
#'
#' @details
#'
#' pdfWeibull(x, k, lambda, c)
#' cdfWeibull(x, k, lambda, c)
#' initialWeibull(x)
#' meanWeibull(k, lambda, c)
#' medianWeibull(k, lambda, c)
#' modeWeibull(k, lambda, c)
#' hazardWeibull(x, k, lamba, c)#'
#'
#' @examples
#' pdf(0:120, 4, 60, 0)
#'
#' @export
pdfWeibull <- function(x, k, lambda, c) {
  k/lambda*((x+c)/lambda)^(k-1)*exp(-((x+c)/lambda)^k)
}

#' @export
drule <- NULL

drule[['dWeibull']] <- sapply(c('x','k','lambda','c'),
                               FUN=function(x) Deriv::Deriv(body(pdfWeibull), x, cache.exp=FALSE),
                               simplify = FALSE)

#' @export
cdfWeibull <- function(x, k, lambda, c) {
  1-exp(-((x+c)/lambda)^k)
}
drule[['qWeibull']] <- sapply(c('x','k','lambda','c'),
                               FUN=function(x) Deriv::Deriv(body(cdfWeibull), x, cache.exp=FALSE),
                               simplify = FALSE)

#' @export
initialWeibull <- function(x) {
  if ('difftime' %in% class(x)) {
    x <- as.numeric(x)
  }
  # starting values: # https://uomustansiriyah.edu.iq/media/lectures/6/6_2021_01_01!11_59_52_PM.pdf
  m <- mean(x, na.rm = TRUE)
  v <- sd(x, na.rm=TRUE)
  k <- (v/m)^-1.086
  #lambda <- m/gamma(1+1/k)
  lambda <- m*k^2.6674/(0.184+0.816*k^2.73855)
  c(k=k, lambda=lambda, c=0)
}


#' @export
meanWeibull <- function(k, lambda, c=0) {
  lambda*gamma(1+1/k) - c
}

.onLoad <- function(libname, pkgname) {
  registerS3method("meanWeibull", "default", meanWeibull, envir = parent.env(environment()))
}

#' @export
medianWeibull <- function(k, lambda, c=0) {
  lambda*log(2)^(1/k) - c
}

#' @export
modeWeibull <- function(k, lambda) {
  ifelse(k<=1, 0, lambda*((k-1)/k)^(1/k)) - c
}

#' @export
hazardWeibull <- function(x, k, lambda, c=0) {
  ifelse(x<0, 0, lambda*k*(x+c)^(k-1)) # to do: not sure about parameter c
}
