#' getLinkInv
#'
#' @param fit a fitted model
#' @param ...
#'
#' @return the name of the link function used in the model
#' @export
#'
#' @examples
#' fit <- glm(count ~ gender, family=poisson, data=data)
#' getLinkInv(fit)
getLinkInv <- function(fit, ...) {
  UseMethod("getLinkInv")
}

#' @export
getLinkInv.default <- function(fit, ...) {
  fit$family$linkinv
}

#' @export
getLinkInv.glmerMod <- function(fit, ...) {
 f <- family(fit)
 f$linkinv
}

#' @export
getLinkInv.coxph <- function(fit, ...) {
 'exp'
}

#' @export
getLinkInv.lmerModLmerTest <- function(fit, ...) { # lmerTest::lmer
  if (!is.null(attr(fit, 'family'))) {
    f <- attr(fit, 'family')
  } else {
    f <- family(fit)
  }
  f$linkinv
}

#' @export
getLinkInv.lmerMod <- function(fit, ...) { # lme4::lmer
  if (!is.null(attr(fit, 'family'))) {
    f<-attr(fit, 'family')
  } else {
    f<-family(fit)
  }
  f$linkinv
}

#' @export
getLinkInv.glmmTMB <- function(fit, component=c('cond', 'zi', 'disp'), ...) {
  component <- match.arg(component)
  f <- getFamily(fit, component=component, ...)
  f$linkinv
}
