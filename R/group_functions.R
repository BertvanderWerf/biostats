# utility functions for ctable

#' count
#' a function usefull for the fun parameter in ctable
#'
#' @param x a vector with values
#' @param na.rm include missing values in the count default FALSE
#' @param na.default return value if x has no values default 0
#'
#' @return the number of non-missing values in vector x
#' @export
#'
#' @examples
#' count(c(1,2,3,4,NA,5))
count <- function(x, na.rm=TRUE, na.default=0) {
  length(x)
}


#' n_missing
#' a function usefull for the fun parameter in ctable
#'
#' @param x a vector with values
#' @param na.rm include missing values in the count default TRUE, otherwise result will be 0 or na.default
#' @param na.default return value if x has no values default 0
#'
#' @returns
#' @export
#'
#' @examples
#' n_missing(c(1,2,3,4,NA, 5))
n_missing <- function(x, na.rm=FALSE, na.default=0) {
  sum(is.na(x))
}


#' first
#' a function usefull for the fun parameter in ctable
#'
#' @param x a vector with values
#' @param na.rm a logical indicating if missing values should be included
#'
#' @return the first element of the vector
#' @export
#'
#' @examples
#' first(1:10)
first <- function(x, na.rm=TRUE) {
  if (na.rm) x <- x[!is.na(x)]
  if (length(x)==0) return(NA)
  return(x[1])
}

#' first
#' a function usefull for the fun parameter in ctable
#'
#' @param x a vector with values
#' @param na.rm a logical indicating if missing values should be included
#'
#' @return the last element of the vector
#' @export
#'
#' @examples
#' last(1:10)
last <- function(x, na.rm=TRUE) {
  if (na.rm) x <- x[!is.na(x)]
  if (length(x)==0) return(NA)
  x[length(x)]
}

#' meanAndSd
#' a function usefull for the fun parameter in ctable
#'
#' @param x a vector with values
#' @param na.rm a logical indicating if missing values should be included
#' @param digits the number of digits to round the values before making a text string
#' @param ... additional parameters, needed for use in function ctable
#'
#' @return a text with 'mean (sd)' with the number of digits specified
#' @export
#'
#' @examples
#' meanAndSd(c(1,2,3,4,NA,5), digits=4)
meanAndSd <- function(x, na.rm=TRUE, digits=2, ...) {
  m <- mean(x, na.rm=na.rm)
  if (is.na(m)) {
    NA
  } else {
    paste0(round(mean(x, na.rm=na.rm), digits)," (",round(sd(x, na.rm=na.rm), digits), ")")
  }
}


#' number_percentage
#' a function usefull as a value for cfun or rfun in ctable
#'
#' @param xi a subset of the vector in x
#' @param x a vector of values
#' @param na.rm if missing values should be included in calculations
#' @param digits the number of significant digits to print for percentage
#'
#' @returns
#' @export
#'
#' @examples
number_percentage <- function(xi, x, na.rm=TRUE, digits=4) {
  if (na.rm==TRUE) {
    ni <- length(xi[!is.na(xi)])
    n <- length(x[!is.na(x)])
  } else {
    ni <- length(xi)
    n <- length(x)
  }

  paste0(ni, ' (',signif(ni/n*100, digits),'%)')
  #paste0(ni,':',n, ' (',signif(ni/n*100, digits),'%)')
}
