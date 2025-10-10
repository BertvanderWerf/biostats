#' getSummary
#'
#' @param data
#' @param maxlevel
#' @param attr
#'
#' @returns
#' @export
#'
#' @examples
getSummary <- function(data, maxlevel=5, attr=NULL) {
  missing <-  function(x, na.rm=FALSE, na.default=0) { sum(is.na(x)) }
  count <- function(x, na.rm=TRUE, na.default=0) {length(x)}

  nLevels <- function(x, na.rm=TRUE, na.default=NA) {
    res <- length(unique(x))
    if (na.rm==TRUE) {
      res <- res[!is.na(res)]
    }
    return(res)
  }

  Levels <- function(x, maxlevel=5) {
    stopifnot(maxlevel>1)
    if (is.factor(x)) {
      nlevels <- nlevels(x)
      if (maxlevel>=nlevels) {
        paste0("(", paste(base::levels(x)[seq_len(nlevels)], collapse=", "), ")")
      } else {
        paste0("(", paste(base::levels(x)[seq_len(maxlevel-1)], collapse=", "), ", ..., ",last(base::levels(x)) ,")")
      }
    } else {
      nlevels <- length(unique(x))
      if (maxlevel>=nlevels) {
        paste0("(", paste(sort(base::unique(x))[seq_len(nlevels)], collapse=", "), ")")
      } else {
        paste0("(", paste(sort(base::unique(x))[seq_len(maxlevel-1)], collapse=", "), ", ..., ", last(sort(base::unique(x))), ")")
      }
    }

  }

  Class <- function(x) { paste(base::class(x), collapse=', ') }

  getAttr <- function(x, which, default=NA) {
    res <- attr(x, which=which)
    if (is.null(res)) return(default)
    res
  }

  tab <- ctable(data, names(data),
                fun=c('class'='Class', 'missing', 'count', total='count','Levels', 'nLevels'),
                missing_na.rm=FALSE, maxlevel=maxlevel,
                total_na.rm=FALSE,
                envir = environment())

  attr <- attr[attr!='levels']
  if (!is.null(attr) && length(attr)>0) {
    extra <- sapply(attr, function(att) sapply(data, getAttr, which=att))
    tab <- cbind(variable= tab$variable, extra, tab[,names(tab)!='variable'])
  }

  tab$variable <- unfactor(tab$variable)
  row.names(tab    ) <- NULL
  tab
}

#data <- set_column_attr(data, 'ID', list(scale='Nominal', purpose='Key',   unit='-'))
#' set_column_attr
#'
#' @param data
#' @param col_name
#' @param attributes
#'
#' @returns
#' @export
#'
#' @examples
set_column_attr <- function(data, col_name, attributes) {
  for (var in names(attributes)) {
    attr(data[[col_name]], var) <- attributes[[var]]
  }
  return(data)
}
