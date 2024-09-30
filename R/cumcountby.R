#' cumcountby
#'
#' calculates the cumulative count within a classification over the values of a
#' variable and stores it in a new variable
#'
#' @param data a dataframe containing the classifying factors and variable name var
#' @param newvar The name of the newly created variable
#' @param var the variable to use as the ordering variable
#' @param fact.names the factor names of the calssification
#'
#' @return the data frame data with a new variable indicating the classification and a new variable indicating the cumulative counts
#' @export
#'
#' @examples
#' data <- cumcountby(data, 'cumcount', 'DurationInDays', c('Sex','Ethnicity'))
cumcountby <- function(data, newvar, var, fact.names) {
  stopifnot(var %in% names(data))
  stopifnot(all(fact.names %in% names(data)))

  for (c in classifications) {
    data[,c] <- factor(data[,c])
  }
  fname <- paste(classifications, collapse='.')
  data[,fname] <- factor(interaction(data[,classifications]))
  data <- data[order(data[,fname], data[,var]),]
  f <- data[,fname]
  data <- split(data, f)
  for (i in 1:nlevels(f)) {
    data[[i]][,newvar] <- c(1:nrow(data[[i]]))/nrow(data[[i]])
  }
  data <- unsplit(data, f)
  data
}
