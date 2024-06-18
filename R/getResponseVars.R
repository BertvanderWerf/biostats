#' getResponseVars
#'
#' @param fit a fitted model e.g. result of glm, coxph, etc.
#' @param data the data used in the model, otherwise it will be derived with getData()
#'
#' @return a character list of response variable names, the attr(,'full.description')
#' will point to the full formula for the left hand side, attr(,'full.list') will
#' list all names and numbers contained in attr(,'full.description')
#' @export
#'
#' @examples
#' fit <- coxph(Surv(duration, dead) ~ age, data=data)
#' getResponseVars(fit)
getResponseVars <- function(fit, data=NULL) {
  if (is.null(data)) {
    data <- getData(fit)
  }
  terms <- getTerms(formula(fit))
  t <- row.names(attr(terms,'factors'))
  t <- t[!(t %in% attr(terms,'term.labels')[attr(terms,'order')==1])]

  res.all <- extractNames(t)
  res <- res.all[res.all %in% names(data)]
  attr(res, 'full.description') <- t
  attr(res, 'full.list') <- res.all

  res
}
