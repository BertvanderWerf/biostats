#' addDiffPValues
#'
#' @param data
#' @param vcov
#' @param df
#' @param estimate
#'
#' @return
#' @export
#'
#' @examples
addDiffPValues <- function(data, vcov, df=NULL, estimate='estimate') {
  allDiffs <- allDifferences(data, vcov, df, estimate)
  allDiffs$P_Values <- as.data.frame(allDiffs$P_Values)
  names(allDiffs$P_Values) <- paste0('P-value.', row.names(data))
  row.names(allDiffs$P_Values) <- row.names(data)
  cbind(data, setZero(allDiffs$P_Values))
}
