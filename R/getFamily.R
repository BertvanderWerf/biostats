
#' getFamily
#'
#' @param fit a fitted model
#'
#' @return the family name of the model
#' @export
#'
#' @examples
#' fit <- glm(count ~ gender, family=binomial, data=data)
#' getFamily(fit)
getFamily <- function(fit) {
  UseMethod('getFamily')
}

#' @export
getFamily.default <- function(fit) {
  return(fit$family$family)
}

#' @export
getFamily.glmerMod <- function(fit) {
  f<-family(fit)
  f$family
}

#' @export
getFamily.coxph <- function(fit) {
  'coxph'
}

#' @export
getFamily.lmerModLmerTest <- function(fit) { # lmerTest::lmer
  if (!is.null(attr(fit, 'family'))) {
    f <- attr(fit, 'family')
  } else {
    f <- family(fit)
  }
  f$family
}

#' @export
getFamily.lmerMod <- function(fit) { # lme4::lmer
  if (!is.null(attr(fit, 'family'))) {
    f<-attr(fit, 'family')
  } else {
    f<-family(fit)
  }
  f$family
}
