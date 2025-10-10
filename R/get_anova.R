#' get_anova
#'
#' @param model
#' @param vcov
#' @param test
#'
#' @returns
#' @export
#'
#' @examples
get_anova <- function(model, vcov=NULL, test='Chisq', use_default=TRUE) {
  if (is.null(vcov)) {
    if (use_default) {
      anova <- as.data.frame(anova(fit, test=test))
      anova$sig <- p_to_stars(anova[,grepl('^Pr(*)', names(anova), fixed=FALSE)])
      anova <- cbind(term=row.names(anova), anova)
      row.names(anova) <- NULL
      return(anova)
    }
    vcov=vcov(fit)
  }
  x <- model.matrix(extractFixedFormula(fit), createPredictors(fit))
  all_names <- colnames(x)
  #all_names <- all_names[all_names!='(Intercept)']
  terms <- attr(terms(fit), 'term.labels')
  if (attr(terms(fit), 'intercept')==1) {
    terms <- c('(Intercept)', terms)
  }
  term <- terms[2]
  res <- lapply(terms, function(term) {
    if (term=='(Intercept)') {
      res <- term
    } else {
      res <- colnames(model.matrix(as.formula(paste('~',term)), data1))
      res <- res[res %in% all_names[all_names!='(Intercept)']]
    }
    hyp <- as.data.frame(car::linearHypothesis(model=fit, hypothesis.matrix=res, vcov=vcov, test=test))[2,]
  } )
  res <- cbind(term=terms, as.data.frame(t(as.matrix(res))))
  res$sig <- p_to_stars(res[,grepl('^Pr(*)', names(res), fixed=FALSE)])
  res <- cbind(terms=row.names(res), res)
  row.names(res) <- NULL
  res
}
