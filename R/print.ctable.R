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
print.ctable <- function(crosstable, sep1='.', sep2=':') {
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

#
# print.ctable <- function(table, htype='minimal', sep1=attr(table, 'hsep')[1],
#                          sep2=attr(table, 'hsep')[2], ...) {
#   htype <- match.arg(htype, c('minimal','maximal'))
#   header <- cols(table)
#   keys <- rows(table)
#   if (htype=='minimal') {
#     cond <- !sapply(header, function(x) { is.null(x) || length(x)==0 || all(x==first(x))})
#     header <- header[,cond, drop=FALSE]
#     cond <- sapply(keys, function(x) { is.null(x) || length(x)==0 || all(x == first(x))})
#     if (any(cond)) {
#       cond <- !cond
#       table <- table[,c(cond, rep(TRUE, nrow(header)))]
#       keys <- keys[,cond, drop=FALSE]
#     }
#     if (ncol(header)==1) {
#       names(table) <- c(names(keys), header[,1])
#     } else {
#       header <- split(header, row.names(header), drop=TRUE)
#       header <- rapply(header, function(x) {levels(x)[x]}, classes='factor', how='replace')
#       new_names <- sapply(header, paste, collapse=sep2)
#       names(table) <- c(names(keys), new_names)
#     }
#     print(as.data.frame(table), row.names=FALSE, na.print=''))
#   }
#
#   names(table) <- c(names(keys), header_to_column_names(header, sep1, sep2))
#   print(table, row.names=FALSE, na.print='')
# }
