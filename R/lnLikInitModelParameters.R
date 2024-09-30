#' lnLikInitModelParameters
#'
#' groups parameter values (mean or weighted mean) according to the (sub)models per parameter
#'
#' @param p
#' @param models
#' @param parnames
#' @param weights
#'
#' @return
#' @export
#'
#' @examples
#' p <0 lnLikInitalParameters(data, 'Ethnicity.Sex.RHD', var='AgeInDays', initialWeibull)
#' fact <- list(k=~1+Ethnicity*Sex, lambda=~1+Ethnicity*RHD, c=~1)
#' par <- lnLikInitModelParameters(p, fact, attr(p, 'parnames', weights='nRecords'))
#'
#' $k
#' (Intercept)         EthnicityITaukei           EthnicityOther                  SexMale EthnicityITaukei:SexMale   EthnicityOther:SexMale
#' 4.540634332             -0.518679006             -0.564422963             -0.112425071              0.085505427              0.005916408
#'
#' $lambda
#' (Intercept)      EthnicityITaukei        EthnicityOther                  RHD1 EthnicityITaukei:RHD1   EthnicityOther:RHD1
#' 25264.71598            -334.40392              74.78567           -1173.46108           -5824.53872           -3892.44643
#'
#' $c
#' (Intercept)
#' 0
lnLikInitModelParameters <- function(p, models, parnames, weights=NULL) {
  for (i in 1:length(fact)) {
    if (i==1) par <- list()
    parname <- parnames[i]
    if (!is.null(models[[i]])) {
      rows <- getFixedVars(fact[[i]], data=p)
      if (is.null(weights)) { # take unweighted means
        mat <- ctable(p, parname, rows, fun='mean')
      } else {
        p$Weighted.par <- p[,weights]*p[,parname]
        mat <- ctable(p, c('Weighted.par',weights), rows, fun='sum')
        mat <- ctable(mat, 'sum', rows=rows, cols='variable', fun='sum')
        mat$mean <- mat$Weighted.par/mat[,weights]
      }
      matx <- model.matrix(fact[[i]], mat)
      par[[i]] <- solve(matx, mat[,'mean'])
    } else {
      par[[i]] <- mean(p[,parname], na.rm=TRUE)
    }
  }
  names(par) <- parnames
  par
}
