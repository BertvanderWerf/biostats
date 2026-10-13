
#' getFamily
#'
#' @param fit a fitted model
#' @param ...
#'
#' @return the family name of the model
#' @export
#'
#' @examples
#' fit <- glm(count ~ gender, family=binomial, data=data)
#' getFamily(fit)
getFamily <- function(fit, ...) {
  UseMethod('getFamily')
}

#' @export
getFamily.default <- function(fit, ...) {
  return(fit$family$family)
}

#' @export
getFamily.glmerMod <- function(fit, ...) {
  f<-family(fit)
  f$family
}

#' @export
getFamily.coxph <- function(fit, ...) {
  'coxph'
}

#' @export
getFamily.lmerModLmerTest <- function(fit, ...) { # lmerTest::lmer
  if (!is.null(attr(fit, 'family'))) {
    f <- attr(fit, 'family')
  } else {
    f <- family(fit)
  }
  f$family
}

#' @export
getFamily.lmerMod <- function(fit, ...) { # lme4::lmer
  if (!is.null(attr(fit, 'family'))) {
    f<-attr(fit, 'family')
  } else {
    f<-family(fit)
  }
  f$family
}

#' @export
getFamily.glmmTMB <- function(fit, component = c("cond", "zi", "disp"),  ...) {

  component <- match.arg(component)

  if (component == "cond") {
    return(family(fit))
  }

  if (component == "zi") {

    ziForm <- fit$modelInfo$allForm$ziformula

    if (is.null(ziForm) ||
        identical(as.character(ziForm)[2], "0")) {
      return(NULL)
    }

    return(binomial(link = "logit"))
  }

  if (component == "disp") {

    dispForm <- fit$modelInfo$allForm$dispformula

    if (is.null(dispForm)) {
      return(NULL)
    }

    ## glmmTMB dispersion models are on a log scale
    return(
      structure(
        list(
          family = "dispersion",
          link = "log"
        ),
        class = "family"
      )
    )
  }
}
