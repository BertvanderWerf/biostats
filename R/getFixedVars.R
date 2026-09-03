#' lhs
#' get the left hand side of a formula
#'
#' @param f a formula
#'
#' @returns the left hand side of the formula if it exists, otherwise NULL
#' @export
#'
#' @examples
#' f <- y ~ a + a/b + (1|c)
#' lhs <- lhs(f)
#' yvars <- all.vars(lhs)
lhs <- function(f) {
  f <- as.formula(f)
  if (length(f)==3) f[[2]]
  else NULL
}

#' rhs
#' get the right hand side of a formula
#'
#' @param f a formula
#'
#' @returns the right hand side of the formula
#' @export
#'
#' @examples
#' f <- y ~ a + a/b + (1|c)
#' rhs <- lhs(f)
#' xvars <- all.vars(rhs)
rhs <- function(f) {
  f <- as.formula(f)
  if (length(f)==3) f[[3]]
  else if (length(f)==2) f[[2]]
  else NULL
}


#' getFixedVars
#'
#' @param fit a fitted model
#' @param data data used, default NULL, extracted from the model with getData
#' @param ...
#'
#' @return a character string with the variable names in the fixed model
#' @export
#'
#' @examples
#' fit <- lmer(count ~ gender*Age + (1|hospital), data=data)
#' getFixedVars(fit)
getFixedVars <- function(fit, data=NULL, ...) {
  UseMethod("getFixedVars")
}

#' @export
getFixedVars.default <- function(fit, data=NULL, ...) {
  # if (is.null(data)) {
  #   data <- getData(fit)
  # }
  formula <- extractFixedFormula(formula(fit))
  all.vars(formula)

  # terms <- getTerms(formula)
  # labels <- labels(terms)
  # unique(unlist(strsplit(labels, "[^[:alnum:]\\._]+")))

}

#' @export
getFixedVars.lnLik <- function(fit, data=NULL, ...) {
  # if (is.null(data)) {
  #   data <- getData(fit)
  # }

  unique(unlist(strsplit(fit$fixed.vars, "[^[:alnum:]\\._]+")))

  #unique(names(data)[sapply(names(data), function(x) any(grepl(paste0('\\b',x,'\\b'), fit$fixed.vars)))])
}

#' @export
getFixedVars.nls <- function(fit, data=NULL, ...) {
  formula <- extractFixedFormula(formula(fit))
  names <- all.vars(formula)
  names[!names %in% names(fit$m$getAllPars())]
}

#' @export
getFixedVars.gls <- function(fit, data=NULL, ...) { # works also for class gnls
  formula <- extractFixedFormula(formula(fit))
  names <- c(all.vars(formula), names(fit$contrasts))
  names[!names %in% names(fit$plist)]
}


#' @export
getFixedVars.glmmTMB <- function(fit, data=NULL,  component=c('cond', 'zi', 'disp'), ...) {
  component <- match.arg(component)

  formula <- extractFixedFormula(getFormula(fit, component))
  all.vars(formula)
}

