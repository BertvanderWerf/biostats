#' getVcov
#'
#' @param fit
#' @param ...
#'
#' @return the name of the link function used in the model
#' @export
#'
#' @examples
#' fit <- glm(count ~ gender, family=poisson, data=data)
#' getVcov(fit)
getVcov <- function(fit, ...) {
  UseMethod("getVcov")
}

#' @export
getVcov.default <- function(fit, ...) {
  vcov(fit, ...)
}

#' @export
getVcov.glmmTMB <- function(fit, component=c('cond', 'zi', 'disp', 'comb'), na.rm=TRUE, ...) {
  component <- match.arg(component)
  if (component=='comb') {
    v <- vcov(fit, full = TRUE)
  } else {
    v <- vcov(fit)
    if (component=='cond' && 'cond' %in% names(v)) {
      v <- v$cond
    } else if (component=='zi' && 'zi' %in% names(v)) {
      v <- v$zi
      colnames(v) <- sub('zi~', '', colnames(v), fixed=TRUE)
      row.names(v) <- colnames(v)
    } else if (component=='disp' && 'disp' %in% names(v)) {
      v <- v$disp
      colnames(v) <- sub('disp~', '', colnames(v), fixed=TRUE)
      row.names(v) <- colnames(v)
    } else {
      v <- NULL
    }
  }
  if (!is.null(v) && isTRUE(na.rm) && any(is.na(v))) {
    cond <- rowSums(is.na(v))==nrow(v)
    v <- v[!cond, !cond, drop=FALSE]
  }

  v
}
