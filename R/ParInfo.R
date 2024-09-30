
#' ParInfo
#'
#' ParInfo prepares the parameter info needed for the lnLik() function
#'
#' @param par a named vector or list with parameter values
#' @param min a vector or list corresponding to parameter par with minimum values for the parameters
#' @param max a vector or list corresponding to parameter par with maximum values for the parameters
#' @param cond a vector or list corresponding to parameter par indicating with 1 estimate or 0 do not estimate parameter
#' @param fact a vecor or list with right sided formula indicating the dependency of the parameter
#' @param link a vector or list with the link-function names for each corresponding parameter
#' @param data a data structure containing the variables used in fact
#' @param sep a separator between parameter name and subgroup
#'
#' @return a list with matching values and with the model matrices in the element mat
#' @export
#'
#' @examples
#' info <- ParInfo(c(k=4, lambda=60, c=0), cond=c(1,1,0), fact=list(~Sex, ~Ethnicity, NULL))
ParInfo <- function(par, min=NULL, max=NULL, cond=NULL, fact=NULL, data=NULL, link=NULL, sep='.')  {

  expand <- function(cond, pars, default) {
    if (!is.null(cond)) {
      cond <- as.list(cond)
      if (length(pars)!=length(cond)) {
        stop('length of ',deparse(cond),' is not equal to the length of the parameters')
      }
      for (i in 1:npars) {
        if (!is.null(cond[[i]])) {
          cond[[i]] <- rep_len(cond[[i]], length(pars[[i]]))
        } else {
          cond[[i]] <- rep_len(default, length(pars[[i]]))
        }
      }
    } else {
      cond <- pars
      for (i in 1:npars) {
        cond[[i]][] <- default
      }
    }
    cond
  }

  pars <- as.list(par)
  npars <- length(pars)
  parnames <- names(pars)
  if (is.null(parnames)) stop('par must be a named vector')
  if (any(parnames=='')) stop('all pars should have a name')

  if (is.null(fact)) {
    fact <- rep(list(~1), npars)
  } else {
    for (i in 1:npars) {
      if (is.null(fact[[i]])) {
        fact[[i]] <- ~1
      }
    }
  }
  stopifnot(all(sapply(fact, class)=='formula'))

  if (is.null(link)) {
    link <- rep(list('identity'), npars)
  } else {
    link <- as.list(link)
    for (i in 1:npars) {
      if (is.null(link[[i]])) {
        link[[i]] <- 'identity'
      }
    }
  }

  if (is.null(data)) {
    mat <- rep(list(NULL), npars)
    warning('the data parameter in ParInfo is NULL, no design matrices are created')
  } else {
    mat <- fact
    for (i in 1:npars) {
      mat[[i]] <- model.matrix(fact[[i]], data=data)
      colnames(mat[[i]]) <- gsub("(", "", gsub(")","", colnames(mat[[i]]), fixed=TRUE), fixed=TRUE)
      pars[[i]] <- rep_len(pars[[i]], ncol(mat[[i]]))
      names(pars[[i]]) <- colnames(mat[[i]])
    }
  }

  cond <- expand(cond, pars, 1)
  min <- expand(min, pars, -Inf)
  max <- expand(max, pars, Inf)
  res <- list(pars=pars,  min=min, max=max, cond=cond, link=link, fact=fact, mat=mat)
  for (j in 1:length(res)) {
    names(res[[j]]) <- parnames
  }

  class(res) <- c('parinfo', class(res))
  res
}

#' @export
summary.parinfo <- function(parinfo) {
  pars <- as.data.frame(sapply(parinfo[1:4], unlist, simplify=FALSE))
  pars <- cbind(parameter= sapply(strsplit(row.names(pars), ".", fixed=TRUE), function(x) x[1], simplify = TRUE),
                parname=row.names(pars), pars)
  row.names(pars) <- NULL

  res <- list()
  res$parameters <- pars
  res$link <- parinfo$link
  res$dependencies <- parinfo$fact
  res
}
