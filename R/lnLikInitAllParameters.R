#' lnLikInitAllParameters
#'
#' calculates the maximum initital parameter estimimates for a certain classification
#'
#' @param data a data frame with the data needed to estimate initial paramters with function fun
#' @param fname the factor indicating the maximal classification used
#' @param var The x-variable used in the function fun
#' @param fun a function which generates the initial paramter estimates e.g. by means of moment estimaters from a series of x values
#'
#' @return it returns a dataframe with classifications, number of observations and parameter estimates for each level of the classification , the attribute attr(.., 'parnames') contains the paremter names
#' @export
#'
#' @examples
#' lnLikInitAllParameters(data, 'Sex.Ethnicity', 'DurationInDays', initialWeibull)
#'                                                Ethnicity.RHD.Sex                Ethnicity RHD    Sex nRecords        k   lambda c
#' Fijian of Indian descent.0.Female Fijian of Indian descent.0.Female Fijian of Indian descent   0 Female    10820 4.545311 26242.35 0
#' ITaukei.0.Female                                   ITaukei.0.Female                  ITaukei   0 Female    18458 4.041471 25285.68 0
#' Other.0.Female                                       Other.0.Female                    Other   0 Female     1282 3.998165 25891.41 0
#' Fijian of Indian descent.1.Female Fijian of Indian descent.1.Female Fijian of Indian descent   1 Female       98 4.024339 24739.13 0
#' ITaukei.1.Female                                   ITaukei.1.Female                  ITaukei   1 Female      264 2.657500 18272.96 0
#' Other.1.Female                                       Other.1.Female                    Other   1 Female       22 2.696943 19419.70 0
#' Fijian of Indian descent.0.Male     Fijian of Indian descent.0.Male Fijian of Indian descent   0   Male    13919 4.428644 24504.75 0
#' ITaukei.0.Male                                       ITaukei.0.Male                  ITaukei   0   Male    20700 4.011723 24613.43 0
#' Other.0.Male                                           Other.0.Male                    Other   0   Male     1823 3.871197 24951.38 0
#' Fijian of Indian descent.1.Male     Fijian of Indian descent.1.Male Fijian of Indian descent   1   Male      125 4.379747 23583.32 0
#' ITaukei.1.Male                                       ITaukei.1.Male                  ITaukei   1   Male      193 2.205246 17466.35 0
#' Other.1.Male                                           Other.1.Male                    Other   1   Male        7 3.480622 22957.25 0
lnLikInitAllParameters <- function(data, fname, var, fun) {
  classifications <- unlist(strsplit(fname, '.', fixed=TRUE))
  tab  <- biostats::ctable(data, names(data)[1], rows=c(fname,classifications), fun=c(nRecords='length'))
  p <- NULL
  for (l in levels(data[,fname])) {
    p_i <- fun(data[data[,fname]==l,var])
    p <- rbind(p, p_i)
  }
  row.names(p) <- levels(data[,fname])
  p <- as.data.frame(p)
  parnames <- names(p)
  p <- cbind(row.names(p), p)
  names(p)[1] <- fname

  i <- match(p[,fname], tab[,fname], nomatch=0)
  res <- cbind(tab, p[i,-1])
  attr(res, 'parnames') <- parnames
  res
}
