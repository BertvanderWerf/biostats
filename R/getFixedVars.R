#' getFixedVars
#'
#' @param fit a fitted model
#' @param data data used, default NULL, extracted from the model with getData
#'
#' @return a character string with the variable names in the fixed model
#' @export
#'
#' @examples
#' fit <- lmer(count ~ gender*Age + (1|hospital), data=data)
#' getFixedVars(fit)
getFixedVars <- function(fit, data=NULL) {
  if (is.null(data)) {
    data <- getData(fit)
  }
  formula <- extractFixedFormula(fit)
  terms <- getTerms(formula)
  labels <- labels(terms)

  unique(names(data)[sapply(names(data), function(x) any(grepl(paste0('\\b',x,'\\b'), labels)))])
}
