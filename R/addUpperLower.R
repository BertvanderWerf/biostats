#' addUpperLower
#'
#' @param pred
#' @param df
#' @param estimate
#' @param variance
#'
#' @return
#' @export
#'
#' @examples
addUpperLower <- function(pred, df=NULL, estimate='estimate', variance='variance', alpha=0.05) {
  lname <- paste('lower',round((1-alpha)*100,0), sep='.')
  uname <- paste('upper',round((1-alpha)*100,0), sep='.')
  alpha <- alpha/2
  if (is.null(df)) {
    pred[,lname] <- setZero(pred[,estimate] + qnorm(alpha) * sqrt(pred[,variance]))
    pred[,uname] <- setZero(pred[,estimate] + qnorm(1-alpha) * sqrt(pred[,variance]))
  } else {
    pred[,lname] <- setZero(pred[,estimate] + qt(alpha, df) * sqrt(pred[,variance]))
    pred[,uname] <- setZero(pred[,estimate] + qt(1-alpha, df) * sqrt(pred[,variance]))
  }
  pred
}
