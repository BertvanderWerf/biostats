
#' getHeader
#'
#' @param colnames a crosstable or names(data.frame))
#' @param sep1 symbol indicating the separation of variables
#' @param sep2 symbol indicating the separation between variable and its level
#'
#' @return a data.frame with the header information
#' @export
#'
#' @examples
getHeader <- function(colnames, sep1=';', sep2=':') {
  if (is.data.frame(colnames)) {
    return(getHeader(names(colnames), sep1, sep2))
  } else if (!is.character(colnames)) {
    stop('the parameter colnames must be a text vector')
  }
  h <- sapply(colnames, function(x) {strsplit(x, sep1)[[1]]}, simplify = FALSE)
  n <- sapply(h, length)
  m <- max(n, na.rm=TRUE)
  if (m==1) {
    Header <- colnames
  } else {
    for (i in 1:length(h)) {
      ni <- length(h[[i]])
      if (ni<m) {
        t <-rep(NA, m)
        t[seq(m-ni+1,m)] <- h[[i]][1]
        h[[i]] <- t
      }
    }
  }
  h <- as.data.frame(t(as.matrix(h)))
  row.names(h) <- NULL

  if (is.na(sep2)) {
    return(h)
  }

  l <- list()
  for (i in 1:ncol(h)) {
    l[[i]] <- getHeader(h[[i]], sep2, NA)
    if (i>1) {
      l[[1]] <- cbind(l[[1]], l[[i]])
    }
  }
  res <- as.data.frame(t(l[[1]]))
  row.names(res) <- NULL
  return(res)
}
