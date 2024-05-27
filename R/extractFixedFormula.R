#' extractFixedFormula
#'
#' @param fit a fitted model
#' @param keep.response Keep the response in the resulting object?
#'
#' @return returns a formula with random terms dropped
#' @export
#'
#' @examples
#' fit <- lmer(count ~ gender*Age + (1|hospital), data=data)
#' extractFixedFormula(fit)
extractFixedFormula <- function(fit, keep.response=FALSE) {
  Terms <- getTerms(fit)
  Random <- grepl("|", labels(Terms), fixed=TRUE)
  stats::drop.terms(Terms, which(Random), keep.response = keep.response)
}
