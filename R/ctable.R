# utility functions for ctable

#' count
#'
#' @param x a vector with values
#'
#' @return the number of non-missing values in vector x
#' @export
#'
#' @examples
#' count(c(1,2,3,4,NA,5))
count <- function(x) { sum(!is.na(x)) }

#' first
#'
#' @param x a vector with values
#' @param na.rm a logical indicating if missing values should be included
#'
#' @return the first element of the vector
#' @export
#'
#' @examples
#' first(1:10)
first <- function(x, na.rm=TRUE) { if (na.rm) x <- x[!is.na(x)]; x[1] }

#' first
#'
#' @param x a vector with values
#' @param na.rm a logical indicating if missing values should be included
#'
#' @return the last element of the vector
#' @export
#'
#' @examples
#' last(1:10)
last <- function(x, na.rm=TRUE) { if (na.rm) x <- x[!is.na(x)]; x[length(x) ]}

#' meanAndSd
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

#' ctable
#'
#' @param data a data frame with data to make a table from
#' @param variable a (named) character vector with the variables to be summarised
#' @param rows a (named) character vector indicating the rows factors of the table
#' @param cols a (named) character vector indicating the column factors of the table
#' @param fun a (named) character vector with the function names
#' @param rdrop a logical indicting if the non existing row combinations should be dropped from the table
#' @param cdrop a logical indicting if the non existing column combinations should be dropped from the table
#' @param na.rm a logical indicating if the missing values should be ignored, the value is populated to the functions in fun if it has na.rm as argument
#' @param hsep a character string of two elements with the separators for the header, the first the separator between factorname and level, the second between the factors
#' @param hkeep a logical indicating to keep all factor names and levels in the header, only used in simple headers
#' @param vpos The position of the variables in the row classification
#' @param vname the name of the column for the variables
#' @param fpos the position of the functions in the column classification
#' @param internal.sep internal separation character used in the function interaction
#' @param ... extra parameters to give to one or more of the functions in fun
#'
#' @return a data.frame of the class ctable with extra attributes for the header and row classifications
#' @export
#'
#' @examples
#' meanAndSd <- function(x, na.rm=TRUE, digits=2, ...) {
#'   m <- mean(x, na.rm=na.rm)
#'   if (is.na(m)) {
#'     NA
#'   } else {
#'     paste0(round(mean(x, na.rm=na.rm), digits)," (",round(sd(x, na.rm=na.rm), digits), ")")
#'   }
#' }
#'#' ctable(data, 'length', rows='species', fun=c('Mean (sd)'='meanAndsd'), digits=3)
#'
#' data(iris)
#' ctable(iris, c('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width'), cols='Species', fun='mean')
ctable <- function(data, variable, rows=NULL, cols=NULL, fun='nObs', rdrop=TRUE,
                   cdrop=TRUE, na.rm=TRUE, hsep=c('.',':'), hkeep=FALSE,
                   vpos=ifelse(length(variable)==1,0,1), vname='variable',
                   fpos=ifelse(length(fun)==1,ifelse(is.null(cols), Inf, 0) ,Inf),
                   internal.sep='\10', ...) {

  setnames <- function(x, val='') {
    if (is.null(x)) return(x)
    stopifnot(is.character(x))

    if (is.null(names(x))) {
      names(x) <- x
    } else {
      names(x) <- ifelse(names(x)==val | is.na(names(x)), x, names(x))
    }
    x
  }

  match.arg.character <- function(arg, choices, several.ok=TRUE){
    arg <- arg[!is.na(arg)]
    if (length(arg)==0) return(NULL)
    if (is.null(arg)) return(NULL)

    if ((length(arg)!=1 & several.ok==FALSE) | !is.character(arg)) {
      stop('argument must be character of length 1 or NULL')
    }
    # store current names, they are lost due to match.arg() function
    names <- rep('',length(arg))
    if (!is.null(names(arg))) {
      names <- ifelse(names(arg)!='',  names(arg), names)
    }

    # check and expand arguments
    arg <- match.arg(arg, choices, several.ok=several.ok)
    if (length(arg)!=length(names)) {
      stop('error in arg: possible multiple choices matches argument or no match at all\nSuggestion: write full argument(s) with proper case')
    }

    # restore names, give names variable of arg
    names(arg) <- names
    names(arg) <- ifelse(names(arg)!='', names(arg), arg)
    arg
  }

  match.arg.logical <- function(arg, choices, several.ok=FALSE) {
    if (is.null(arg)) return(choices[1])
    if ((length(arg)!=1 & several.ok==FALSE) | !is.logical(arg)) {
      stop('argument must be logical of length 1 or NULL')
    }
    arg[is.na(arg)] <- choices[1]
    arg
  }

  if (!is.null(variable)) variable <- match.arg.character(variable, names(data), TRUE)
  if (!is.null(rows)) rows <- match.arg.character(rows, names(data), TRUE)
  if (!is.null(cols)) cols <- match.arg.character(cols, names(data), TRUE)
  rdrop <- match.arg.logical(rdrop, c(FALSE,TRUE))
  cdrop <- match.arg.logical(cdrop, c(FALSE,TRUE))
  hkeep <- match.arg.logical(hkeep, c(TRUE, FALSE))
  if (!is.character(fun)) {
    stop('the argument "fun" must be a (named) character vector')
  }
  fun <- setnames(fun)

  if (is.null(rows)) {
    row.groups <- factor(rep(1, nrow(data)))
  } else {
    row.groups <- interaction(data[,rows], sep=internal.sep, drop=rdrop)
  }
  nrows <- nlevels(row.groups)
  row.names <- levels(row.groups)

  if (is.null(cols)) {
    col.groups <- factor(rep(1, nrow(data)))
  } else {
    col.groups <- interaction(data[,cols], sep=internal.sep, drop=cdrop)
  }
  ncols <- nlevels(col.groups)
  col.names <- levels(col.groups)
  row.groups <- split(row.groups, col.groups)

  # create groups
  l<-strsplit(levels(row.groups[[1]]), internal.sep, fixed=TRUE)
  groups <- as.data.frame(t(as.matrix(l, equal = FALSE)))
  if (!is.null(rows)) {
    names(groups) <- rows
    groups <- biostats::copy.type(groups, data, rows) # to do: test
    names(groups) <- names(rows)
    vars <- factor(rep(names(variable), rep(nrow(groups), length(variable))), names(variable))
    groups <- cbind(vars, groups)
  } else {
    groups=data.frame(factor(variable, variable))
  }
  names(groups)[1] <- vname

  if (vpos>1 & !is.null(rows)) {
    vpos <- pmin(ncol(groups), vpos)
    if (vpos==ncol(groups)) groups <- groups[,c(2:ncol(groups),1)]
    else  groups <-groups[,c((2:vpos), 1, ((1+vpos):ncol(groups)))]
  }

  # create header
  function.name <- 'function'
  if (!is.null(cols)) {
    header <- as.data.frame(t(as.matrix(strsplit(levels(col.groups[[1]]), internal.sep, fixed=TRUE))))
    names(header) <- cols
    header <- copy.type(header, data, cols)
    names(header) <- names(cols)
    if (function.name %in% names(header)) {
      i <- 2; while ((function.name<-paste0('function_',i)) %in% names(header)) { i <- i+1 }
    }
    tempvar <- factor(rep(names(fun), rep(nrow(header), length(fun))), names(fun))
    header <- cbind(tempvar, header)
  } else {
    header <- data.frame(factor(names(fun), names(fun)))
  }
  names(header)[1] <- function.name

  if (fpos<=0 & (is.null(cols) | length(fun)>1))  {
    warning('fpos is set to 0 while length(fun)>1 or is.null(cols): the fpos parameter is reset to Inf')
    fpos <- Inf
  }

  if (fpos>1 & ! is.null(cols)) {
    fpos <- pmin(ncol(header), fpos)
    if (fpos==ncol(header)) {
      header <- header[,c(2:ncol(header),1)]
    } else {
      header <-header[,c((2:fpos), 1, ((1+fpos):ncol(header)))]
    }
  } else if (fpos<=0 & !is.null(cols)) { # remove
    header <- header[,-1, drop=FALSE]
  }

  m <- matrix(NA, nrow=nrows, ncol=ncols) # output template
  for (i in 1:length(variable)) {
    data_i <- split(data[,variable[i]], col.groups)
    for (j in 1: length(data_i)) {
      data_i[[j]] <- split(data_i[[j]], row.groups[[j]])
    }
    for (j in 1:length(fun)) {
      f <- get(fun[j])

      temp <- m
      if ('...' %in% names(formals(f))) {
     #   if ("na.rm" %in% names(formals(f)))
          res <- rapply(data_i, f=f, na.rm=na.rm, ...)
        # else {
        #   res <- rapply(data_i, f=f, ...)
        # }
      } else {
        if ('na.rm' %in% names(formals(f))) {
          res <- rapply(data_i, f=f, na.rm=na.rm)
        } else {
          res <- rapply(data_i, f=f)
        }
      }
      if (length(res)==length(m)) {
        temp[] <- res
      } else {
        stop('The function "',fun[j], '" causes an error, does it return more than one value?')
      }
      if (j==1) {
        table_i <- temp
      } else {
        table_i <- cbind(table_i, temp)
      }
    }
    if (i==1) {
      table <- table_i
    } else {
      table <- rbind(table, table_i)
    }
  }

  for (i in 1:ncol(header)) {
    if (i==1) {
      bStart<- TRUE
      titles <- NULL
    }
    if (names(header)[i]==function.name | hkeep==FALSE) {
      if (hkeep==FALSE) {
        if (all(header[1,i]==header[,i])) {
          t <- NULL
        } else {
          t <- header[,i]
        }
      } else {
        t <- header[,i]
      }
    } else {
      t <- paste(names(header)[i],header[,i], sep=hsep[1])
    }
    if (!is.null(t)) {
      if (bStart) {
        titles <- t
        bStart <- FALSE
      } else {
        titles <- paste(titles, t, sep=hsep[2])
      }
    }
  }
  if (is.null(titles)) {
    titles <- header[1,1]
  }

  table <- as.data.frame(table)
  names(table) <- titles

  #colindex <- do.call(order, header[,c(names(header)[ncol(header)], names(header)[-ncol(header)]), drop=FALSE])
  colindex <- do.call(order, header[,names(header), drop=FALSE])
  header <- header[colindex,, drop=FALSE]
  table <- table[,colindex, drop=FALSE]

  table <- cbind(groups, table)
  table <- table[do.call(order, table[,names(groups), drop=FALSE]),]
  row.names(table) <- NULL


  if (vpos<=0) { # remove variable
    if (length(variable)>1) {
      warning('vpos is set to 0 while length(variable)>1: the vpos parameter is reset to 1')
    } else {
      table <- table[,-1, drop=FALSE]
    }
  }

  attr(table, 'header') <- header
  attr(table, 'groups') <- groups

  class(table) <- c('ctable', class(table))

  t <- table(names(table))
  if (any(t>1)) {
    warning('columns with the same header found for ',paste0("(", paste(names(t)[t>1], collapse=', '),"); use option hkeep=TRUE or rename with named variable"))
  }
  return(table)
}
