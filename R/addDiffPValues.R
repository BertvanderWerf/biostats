#' addDiffPValues
#'
#' @param data
#' @param vcov
#' @param df
#' @param estimate
#' @param method the default method is no corrections see stats::p.adjust
#'
#' @return
#' @export
#'
#' @examples
addDiffPValues <- function(data, vcov, df=NULL, estimate='estimate', method='none') {
  allDiffs <- allDifferences(data, vcov, df, estimate)
  if (method=='none') {
    allDiffs$P_Values <- as.data.frame(allDiffs$P_Values)
  } else {
    mat <- allDiffs$P_Values*0
    mat[lower.tri(mat)] <- stats::p.adjust(allDiffs$P_Values[lower.tri(allDiffs$P_Values)],
                                           method, n = nrow(mat) * (nrow(mat) - 1)/2)
    mat <- mat + t(mat)
    diag(mat) <- 1
    allDiffs$P_Values <- as.data.frame(mat)
  }
  names(allDiffs$P_Values) <- paste0('P-value.', row.names(data))
  row.names(allDiffs$P_Values) <- row.names(data)
  cbind(data, setZero(allDiffs$P_Values))
}
