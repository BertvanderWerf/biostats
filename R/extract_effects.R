#' extract_effects
#' @title Extract structured effects from parameter names
#'
#' @param coef_name the names of the coefficients as returned by names(coefficients(model))
#' @param factors a data.frame with the factors used to define the parameters
#' @param parname the names of the resulting column containing the paramters
#'
#' @description Handles parameter strings containing both base parameter names (optional) and
#'              factor levels (e.g. "beta.treatmentHigh") the general format of the parameterstring is
#'              [parameter_name.]factor1_namefactor1_level[:factor2_namefactor2_level...]
#'              or [parameter_name]
#'
#' @return A data.frame with columns:
#' \itemize{
#'   \item{parametername}{Base parameter name}
#'   \item{[factor columns]}{Extracted levels for each specified factor}
#' }
#'
#' @examples
#' to do:
#'
#' @export
extract_effects <- function(coef_names, factors, parname='Parameter') {
  stopifnot(is.list(factors) | is.data.frame(factors))
  stopifnot(all(sapply(factors, is.factor)))
  if (length(factors)==0) {
    mat <- data.frame(factor(coef_names))
    names(mat)[1] <- parname
    return(mat)
  }

  factor_names <- names(factors)

  # Build all possible factor+level strings
  combos <- sapply(names(factors), function(x) paste0(x,levels(factors[,x])), USE.NAMES=TRUE, simplify = FALSE)
  #combos <- as.data.frame(combos, stringsAsFactors = FALSE)
  combos <-do.call(rbind, lapply(factor_names, function(name) { data.frame(factor=name,
                                                                           level=levels(factors[[name]]),
                                                                           string=combos[[name]],
                                                                           stringsAsFactors = FALSE)}))

  mat <- matrix(NA_character_, nrow=length(coef_names), ncol=length(factors), dimnames=list(NULL, names(factors)))
  par <- rep(NA_character_, length(coef_names))

  p <- strsplit(coef_names, ":", fixed=TRUE)
  i <- 2
  j <- 1
  for (i in 1:length(p)) {
    for (j in 1:length(p[[i]])) {
      x <- p[[i]][j] # one factornameLevel combination or Parametername.factornameLevel when j==1
      if (j==1) {
        c <- endsWith(x, combos$string)
      } else {
        c <- x==combos$string
      }
      if (sum(c)==1) {
        mat[i,combos$factor[c]] <- combos$level[c]
        if (j==1) {
          par[i] <- sub(combos$string[c], "", x, fixed=TRUE)
          if (par[i]!='') {
            par[i] <- substr(par[i], 1, nchar(par[i])-1)
          } else {
            par[i] <- NA_character_
          }
        }
      } else if (sum(c)==0) {
        if ((j==1)&(length(p[[i]])==1)) { # only parname
          par[i] <- x
        } else {
          stop('fault at ',x,' format of coef_names does not match the required one, factor level does not exist or two factor/level leads to same string')
        }
      } else { # sum(c)>1
        stop('fault at ',x,' format of coef_names does not match the required one, factor level does not exist or two factor/level leads to same string')
      }
    }
  }
  mat <- as.data.frame(mat, stringsAsFactors = FALSE)
  mat <- copy.type(mat, factors, names(factors))
  if (!all(is.na(par))) {
    par <- factor(par)
    mat <- cbind(par, mat)
    names(mat)[1] <- parname
  }
  mat
}
