

#' Control of lnLik model fitting
#'
#' @description
#' Construct control structures for lnLik model fitting. All arguments have defaults
#'
#'
#' @param method convergence method to use
#' @param criterium the stop criterium
#' @param eps the value when reached stops the iterations
#' @param nsteps the maximum number of steps in iteration
#' @param hessian which hessian to use, second order derived with Deriv, or expected one
#' @param lambda the lambda value in levenberg
#' @param lambda.multiplier the lambda multiplier in levenberg-marquardt
#' @param reduce A logical indicating to reduce the dataset if possible
#' @param replaceNAN replace NAN and Inf with zeros in derivatives
#' @param useGInv use generalised inverse
#'
#' @return The control function return a list containing
#' 1. general control parameters, method, hessian type
#' 2. optimisation control: criterium, eps, nsteps
#' @export
#'
#' @examples
#' str(lnLikControl())
lnLikControl <- function(method=c('NewtonRaphson','Levenberg','Marquardt'),
                        criterium=c('delta','jacobian','lnlik'), eps=1e-7, nsteps=200,
                        hessian=c('calculate','expected'), lambda=1, lambda.multiplier=10,
                        reduce=TRUE, replaceNAN=FALSE, useGInv=FALSE) {

  method <- match.arg(method)
  hessian <- match.arg(hessian)
  criterium <- match.arg(criterium, several.ok = TRUE)
  list(method=method, criterium=criterium, hessian=hessian, eps=eps, nsteps=nsteps,
       lambda=lambda, lambda.multiplier=lambda.multiplier, reduce=reduce,
       replaceNAN=replaceNAN, useGInv=useGInv)
}
