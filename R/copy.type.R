
#' copy.type
#'
#' @param newdata a data matrix with columns varnames which has to match types of data[,varnames]
#' @param data a data matrix from which to copy the data type
#' @param varnames the names of the columns to check
#'
#' @return a data matrix with matching data types and levels for varnames with data[,varnames]
#' @export
#'
#' @examples
#' copy.type(estimates, data, c('AgeGroup','Ethnicity'))
copy.type <- function(newdata, data, varnames) {
  varnames <- match.arg(varnames, names(data), TRUE)
  varnames <- match.arg(varnames, names(newdata), TRUE)
  for (var in varnames) {
    if (is.factor(data[,var])) {
      newdata[,var] <- factor(newdata[,var], levels(data[,var]))
    } else if (is.integer(data[,var])) {
      newdata[,var] <- as.integer(newdata[,var])
    } else if (is.numeric(data[,var])) {
      newdata[,var] <- as.numeric(newdata[,var])
    } else if (base::is.logical(data[,var])) {
      newdata[,var] <- as.logical(newdata[,var])
    } else if ('POSIXc' %in% class(data[,var])) {
      newdata[,var] <- as.POSIXlt(newdata[,var])
      # } else if ('difftime' %in% class(data[,var])) {
      #   newdata[,var] <- as.difftime(d, units=units(data[,var]))
    } else if ('Date' %in% class(data[,var])) {
      newdata[,var] <- as.Date(newdata[,var])
    }
  }
  return(newdata)
}
