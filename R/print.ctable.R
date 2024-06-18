#' print.ctable
#'
#' @param crosstable
#' @param sep1
#' @param sep2
#'
#' @return
#' @export
#'
#' @examples
print.ctable <- function(crosstable, sep1=';', sep2=':') {
  stopifnot('ctable' %in% class(crosstable))
  h <- getHeader(crosstable, sep1=sep1, sep2=sep2) # to do: include in package
  n <- nrow(h)
  if (n>0) {
    names(crosstable) <- names(h)
    ni1 <- sapply(h, function(x) { pmax(1,max(nchar(as.character(x), keepNA=FALSE), na.rm=TRUE), na.rm=TRUE)})
    ni2 <- sapply(crosstable, function(x) { pmax(1,max(nchar(as.character(x), keepNA=FALSE), na.rm=TRUE), na.rm=TRUE)})
    ni <- pmax(ni1, ni2, na.rm=TRUE)
    h <- rbind(h, strrep('-', as.list(ni)))

    crosstable <- rbind(h,crosstable)
    names(crosstable) <- NULL
  }
  print(crosstable, row.names=FALSE, na.print='')
}
