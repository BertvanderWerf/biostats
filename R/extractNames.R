#' extractNames
#'
#' @param x input text to split
#' @param split punctuation characters, used for splitting the text recursively
#'
#' @return names and numbers contained in x, all punctuation (except _ and .) characters are removed
#' @export
#'
#' @examples
#' extractNames('Surv(Duration/365.25, Dead)')
extractNames <- function(x, split='!"#$%&\'()*+,-/:;<=>?@[\\]^`{|}~ ') {
  t <- x
  sapply(strsplit(split, '')[[1]], function(x) { t <<- unlist(strsplit(t, x, fixed=TRUE)) })
  t[t!='']
}
