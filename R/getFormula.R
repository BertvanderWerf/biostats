
#' getFormula
#'
#' @param fit fitted model
#' @param ...
#'
#' @return the original dataset used in the regression
#' @export
#'
#' @examples
#' fit <- lm(y ~ x, data=data)
#' getData(fit)
getFormula <- function(fit, ...) {
  UseMethod("getFormula")
}

#' @export
getFormula.default <- function(fit, ...) {
  formula(fit)
}

#' @export
getFormula.gnls <- function(fit, ...) {
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

#' @export
getFormula.glmmTMB <- function(fit, component=c('cond', 'zi', 'disp'), ...) {
  remove_offset <- function(f) {
    drop_offset <- function(x) {
      if (!is.call(x))
        return(x)

      if (identical(x[[1]], quote(offset)))
        return(NULL)

      if (identical(x[[1]], quote(`+`))) {
        lhs <- drop_offset(x[[2]])
        rhs <- drop_offset(x[[3]])

        if (is.null(lhs)) return(rhs)
        if (is.null(rhs)) return(lhs)

        return(call("+", lhs, rhs))
      }

      x
    }

    rhs_pos <- if (length(f) == 3L) 3L else 2L
    f[[rhs_pos]] <- drop_offset(f[[rhs_pos]])

    f
  }
  component <- match.arg(component)
  remove_offset(formula(fit, component=component))
}

