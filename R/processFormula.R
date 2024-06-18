#' processFormula
#'
#' @param formula a formula or a text representing a formula
#'
#' @return left and right hand site variables and terms and random variables or terms
#' @export
#'
#' @examples
#' processFormula('cbind(n, population) ~ Sex + log(Age) + (1|hospital)')
processFormula <- function(formula) {
  make_formula <- function(str) {
    as.formula(paste('~', paste(str, collapse='+')))
  }

  extract_vars <- function(str) {
    if (is.null(str)) {
      return(NULL)
    }
    if (length(str)==0) {
      return(NULL)
    }
    res <- all.vars(make_formula(str))
    if (length(res)==0) {
      return(NULL)
    }
    return(res)
  }

  if (is.character(formula)) {
    f <- as.character(formula(formula))
  } else if ("formula" %in% class(formula)) {
    f <- as.character(formula(formula))
  }

  if (length(f) == 2) {
    if (f[1] != '~') {
      stop("Unexpected formula structure detected. Please check the input.")
    }
    f <- c(f[1],'', f[2])
    lhs <- NULL
  } else if (length(f) != 3 || f[1] != "~") {
    stop("Unexpected formula structure detected. Please check the input.")
  } else {
    lhs <- extract_vars(f[2])
  }
  rhs <- extract_vars(f[3])

  Terms <- terms(formula(formula))

  terms <- attr(Terms, 'term.labels')
  cond <- grepl("|", terms, fixed=TRUE)
  if (any(cond)) {
    rterms <- extract_vars(terms[cond])
    rhs <- extract_vars(terms[!cond])
  } else {
    rterms <- NULL
  }

  terms <- as.character(attr(Terms, "variables"))[-1]
  LHS <- NULL
  RHS <- attr(Terms,'term.labels')
  if (length(RHS)==0) {
    RHS <- NULL
  }
  RTERMS <- NULL
  if (length(terms)>0) {
    i.response <-attr(Terms, 'response')
    i.response <- i.response[i.response!=0]
    if (length(i.response)==0) {
      LHS <- NULL
    } else {
      LHS <- terms[i.response]
    }
    if (!is.null(RHS)) {
      cond <- grepl("|", RHS, fixed=TRUE)
      if (any(cond)) {
        RTERMS <- RHS[cond]
        RHS <- RHS[!cond]
      } else {
        RTERMS <- NULL
      }
    }
  }

  list(formula=formula(formula), lhs.vars=lhs, rhs.vars=rhs, random.vars=rterms,
       lhs.terms=LHS, rhs.terms=RHS, random.terms=RTERMS, intercept=attr(Terms,'intercept')==1)
}
