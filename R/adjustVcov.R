
#' adjustVcov
#'
#' @param vcov
#' @param tol
#'
#' @return
#' @export
#'
#' @examples
adjustVcov <- function(vcov) {
  vcov <- (vcov+t(vcov))/2
  setZero(vcov)
}
