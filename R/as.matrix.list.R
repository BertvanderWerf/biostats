#' as.matrix.list
#'
#' @param l a list of vectors
#' @param equal a boolean indicating if the vectors in the list should have equal lengths
#'
#' @return a matrix with the vectors in each column, if equal==FALSE the lengths will be adjusted with NA's
#' @export
#'
#' @examples
#' l <- list(a=c(1,2,3), b=1:10)
#' as.matrix(l, equal=FALSE)
as.matrix.list <- function(l, equal=TRUE) {
  l1 <- l
  bUnlist <- is.list(l1[[1]]) &(!is.data.frame(l1[[1]]))
  while (bUnlist) {
    n <- sapply(l1,length, simplify = TRUE)
    stopifnot(all(n==n[1]))
    l1 <-unlist(l1, recursive=FALSE)
    bUnlist <- is.list(l1[[1]])
  }
  n <- sapply(l1,length, simplify = TRUE)
  if (!(all(n==n[1]))) {
    if (equal==TRUE) {
      stop('not all lengths of elements of list are of equal length')
    } else {
      m <- max(n)
      l1 <- sapply(l1, function(x) { c(x,rep(NA,m))[1:m]}, simplify = FALSE)
    }
  }
  m <- matrix(unlist(l1), ncol=length(l1), byrow=FALSE,
              dimnames=list(names(l1[[1]]), names(l1)))
  return(m)
}
