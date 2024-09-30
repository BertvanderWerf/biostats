
#' bdiag
#'
#' @param L
#'
#' @return
#' @export
#'
#' @examples
bdiag <- function(L) {
  stopifnot(all(!sapply(L, is.null)))
  dims <- sapply(L, dim)
  lower.i <- cumsum(c(1, dims[1,]))[1:ncol(dims)]
  upper.i <- lower.i+dims[1,]-1
  lower.j <- cumsum(c(1, dims[2,]))[1:ncol(dims)]
  upper.j <- lower.j+dims[2,]-1
  m <- matrix(0, max(upper.i), max(upper.j))
  names <- c()
  for (k in 1:length(L)) {
    i <- seq(lower.i[k], upper.i[k])
    j <- seq(lower.j[k], upper.j[k])
    m[i,j] <- as.matrix(L[[k]])
    names <- c(names, paste(names(L)[k], colnames(L[[k]]), sep='.'))
  }
  colnames(m) <- names
  m
}
