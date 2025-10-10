#' p_to_stars
#'
#' @param P vector or scalar with P-values
#' @param alpha default value alpha=c('*'=0.05,'**'=0.01,'***'=0.001,"."=0.1)
#'
#' @return text vector of length P with level of siginficances
#' @export
#'
#' @examples
p_to_stars <- function (P, alpha=c('*'=0.05,'**'=0.01,'***'=0.001,"."=0.1)) {
  stopifnot(all(names(alpha)!=''))
  res <- rep('n.s.', length(P))
  cond <- is.na(P)
  if (any(cond)) res[cond] <- NA
  if (length(res)>0) {
    alpha <- alpha[order(alpha, decreasing = TRUE)] # preserves names, sort doesn't
    for (i in 1:length(alpha)) {
      cond <- (P<=alpha[i]) & (!is.na(P))

      if (any(cond)) res[cond] <- names(alpha)[i]
    }
  }
  return(res)
}
