
#' getFormula
#'
#' @param fit fitted model
#'
#' @return the original dataset used in the regression
#' @export
#'
#' @examples
#' fit <- lm(y ~ x, data=data)
#' getData(fit)
getFormula <- function(fit) {
  UseMethod("getFormula")
}

#' @export
getFormula.default <- function(fit) {
  formula(fit)
}

#' @export
getFormula.gnls <- function(fit) {
  res <- formula(fit)

  c <- fit$call
  extracted_function <- get(as.character(c[[1]]))
  matched_call <- match.call(extracted_function, c)
  args <- as.list(matched_call)[-1]
  params <- eval(args$params)

  Params <- list()
  for (i in 1:length(params)) {
    F <- params[[i]]
    parnames <- all.vars(lhs(F))
    f <- as.formula(paste("~ ", deparse(rhs(F))))
    for (parname in parnames) {
      Params[[parname]] <- as.formula(paste(parname, deparse(rhs(F)), sep='~'))
    }
  }
  # make sure names of Params and plist mach names in fit in same order
  parNames <- names(fit$plist)
  attr(res, 'params') <- Params[match(names(Params), parNames)]
  res
}
