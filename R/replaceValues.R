#' replacevalues
#'
#' @description
#' replaces values in a vector/factor with new values. If the inputvector is a factor thant the levels of the factor will be adjusted.
#' Replacevalues can be used to swap values e.g. changing the order in likert scale, values are not recycled!
#'
#' @param vec vector with values, can be a factor
#' @param oldvalues oldvalues to change, there shouldn't be double values in oldvalues
#' @param newvalues newvalues to replace matching oldvalues, length of newvalues must be equal to length of oldvalues or 1
#' @param warnings if a value in oldvalue is not in vec, a warning is given
#' @param droplevels if after replacement values are not in a factor then those levels will be dropped according to the value of droplevels (TRUE, FALSE)
#'
#' @return a vector or factor with new values, the levels of the factor will be expanded with new values (if the corresponding old value is in original ved) and old values will be dropped
#' @export
#'
#' @examples
#'
#' replacevalues(vec=factor(c('x',NaN,NA,NA,'y')), oldvalues=c(NaN, NA), newvalues=c('Q')) # missing level Q will be added
#' replacevalues(vec=factor(c('x',NaN,NA,NA,'y')), oldvalues=c('d'), newvalues=c('Q'))  # level Q will not be added
#'
replacevalues <- function (vec, oldvalues, newvalues, warnings = TRUE, droplevels = TRUE) {
  stopifnot(length(oldvalues)==length(unique(oldvalues)))
  stopifnot(length(oldvalues)>0 & length(newvalues)>0)
  if (is.factor(oldvalues)) oldvalues <- unfactor(oldvalues)
  if (is.factor(newvalues)) newvalues <- unfactor(newvalues)
  no <- length(oldvalues)
  nn <- length(newvalues)
  if (no != nn) {
    if (no==1) {
      stop('length of oldvalues cannot be less than length of newvalues')
    } else if (nn==1) {
      newvalues <- rep(newvalues, no)
      nn <- no
    } else {
      stop("The length of newvalues should be 1 or having the same length as oldvalues")
    }
  }

  if (length(unique(oldvalues)) != no) {
    doubles <- as.matrix(table(oldvalues, useNA = "ifany"))
    doubles <- row.names(doubles)[doubles[, 1] > 1]
    if (length(doubles) == 1) {
      stop("The value ", doubles, " is found multiple times in oldvalues, oldvalues must have unique values")
    } else {
      stop("The values (", paste(doubles, collapse = ", "),
           ") are found multiple times in oldvalues, oldvalues must have unique values")
    }
  }

  if (is.factor(vec)) {
    newlevels <- replacevalues(levels(vec), oldvalues, newvalues, warnings=FALSE)
    levels(vec) <- newlevels

    i <- match(vec, oldvalues, nomatch = 0)
    c <- i > 0
    i <- i[c]

    cond <- !(newvalues %in% newlevels)
    if (any(cond) & (length(i)>0)) {
      levels(vec) <- c(levels(vec),newvalues[cond])
    }
    vec[c] <- newvalues[i]
    if (droplevels==TRUE) {
      vec <- droplevels(vec)
    }
  } else {
    if (warnings) {
      notfound <- match(oldvalues, vec, nomatch = 0)
      notfound <- oldvalues[notfound == 0]
      if (length(notfound) > 0) {
        if (length(notfound) == 1) {
          warning("The value ", notfound, " is not found in vec")
        }
        else {
          warning("The values (", paste(notfound,
                                        collapse = ", "), ") are not found in vec")
        }
      }
    }
    i <- match(vec, oldvalues, nomatch = 0)
    c <- i > 0
    i <- i[c]
    vec[c] <- newvalues[i]
  }
  return(vec)
}
