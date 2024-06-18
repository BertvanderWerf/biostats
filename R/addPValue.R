#' addPvalue
#'
#' @param pred
#' @param df
#' @param hypot
#' @param estimate
#' @param variance
#'
#' @return
#' @export
#'
#' @examples
addPvalue <- function (pred, df=NULL, hypot=0, estimate='estimate', variance='variance') {
  z <- (pred[,estimate]-hypot)/sqrt(pred[,variance])
  if (is.null(df)) {
    pred$P_value <- (1 - pnorm(abs(z))) * 2
  } else {
    pred$P_value <- (1 - pt(abs(z), df)) * 2
  }
  pred$P_value <- setZero(ifelse(pred[,variance]==0, 1, pred$P_value))
  pred
}
