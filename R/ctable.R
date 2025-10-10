#' @noRd
#' utility function for ctable
ctable_rapply_function <- function(data_i, f, fun_name, na.rm = TRUE, ...) {
  f_args <- as.list(formals(args(f)))
  if("na.rm" %in% names(f_args)) f_args$na.rm <- na.rm
  na.default <- if('na.default' %in% names(f_args)) f_args$na.default else NA

  f_args <- replace_args(f=f_args, fname=fun_name, ...)

  handle_empty <- function(x) {
    if ('na.rm' %in% names(f_args)) {
      na.rm <- f_args$na.rm
    }
    if (na.rm == TRUE) x <- x[!is.na(x)]
    if (length(x) == 0) {
      na.default
    } else {
      arg <- f_args[[1]]
      if (!base::missing(arg)) {
        warning(
          sprintf("%s argument %s of %s has already a value, this value will be overwritten",
                  error_string(paste0('fun=',fun_name)), names(f_args)[1], fun_name)
        )
      }
      f_args[[1]] <- x

      found <- rep(FALSE, length(f_args))
      for (i in 1:length(f_args)) {
        if (names(f_args)[i]!='...') {
          arg <- f_args[[i]]
          found[i] <- base::missing(arg)
        }
      }
      if (any(found)) { # to do: make better error message
        stop(
          sprintf("%s argument %s of %s has no value.\nDid you assign a function for cfun or rfun to fun?",
                  error_string(paste0('fun=',fun_name)), names(f_args)[found], fun_name),
          call. = FALSE
        )
      }
      do.call(f, as.list(f_args))
    }
  }

  return(rapply(data_i, f = handle_empty, how = 'list'))
}
# tab <- ctable(data, variable='Nurse_contacts.hospice', rows='GenderDesc',  fun=c('count',M='n_missing'), M_na.rm=FALSE)
# tab


#' @noRd
#' unility function for ctable
ctable_mapply_function <- function(f, fun_name, d_ij, d_i, na.rm=TRUE, ...) {

  f_args <- as.list(formals(args(f)))
  if("na.rm" %in% names(f_args)) f_args$na.rm <- na.rm
  na.default <- if('na.default' %in% names(f_args)) f_args$na.default else NA

  f_args <- replace_args(f=f_args, fname=fun_name, ...)

  handle_empty <- function(xi, x) {
    if ('na.rm' %in% names(f_args)) {
      na.rm <- f_args$na.rm
    }
    if (na.rm==TRUE) {
      xi <- xi[!is.na(xi)]
      x <- x[!is.na(x)]
    }
    if (length(xi)==0 | length(x)==0) {
      na.default
    } else {
      f_args[[1]] <- xi
      f_args[[2]] <- x

      do.call(f, as.list(f_args))
    }
  }
  return(mapply(FUN=function(xi, x) { mapply(handle_empty, xi, x, SIMPLIFY = FALSE) }, d_ij, d_i, SIMPLIFY = FALSE))
}

#' @noRd
#' unility function for ctable
error_string <- function(parname) {
  paste0('ctable(... ,',parname,' = , ...)')
}


#' ctable
#'
#' @param data a data frame with data to make a table from
#' @param variable a (named) character vector with the variables to be summarised
#' @param rows a (named) character vector indicating the rows factors of the table
#' @param cols a (named) character vector indicating the column factors of the table
#' @param fun a (named) character vector with the function names, default 'count'
#' @param rdrop a logical indicting if the non existing row combinations should be dropped from the table
#' @param cdrop a logical indicting if the non existing column combinations should be dropped from the table
#' @param na.rm a logical indicating if the missing values should be ignored, the value is populated to the functions in fun if it has na.rm as argument
#' @param hsep a character string of two elements with the separators for the header, the first the separator between factorname and level, the second between the factors
#' @param vname the name of the column for the variables
#' @param vpos The position of the variables in the row classification
#' @param fname the name for the functions
#' @param fpos the position of the functions in the column classification
#' @param rmissing The factor level name for missing row factor values
#' @param cmissing The factor level name for missing column factor values
#' @param as.data.frame Logical indicating whether to return a plain data.frame instead of a ctable object (default: TRUE)
#' @param header_minimal Logical controlling header simplification when `as.data.frame = TRUE` (default: TRUE)
#' @param envir environment to evaluate the function names in fun, rfun and cfun default .GlobalEnv
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
ctable <- function(data, variable, rows=NULL, cols=NULL, fun=NULL, rfun=NULL, cfun=NULL, rdrop=TRUE,
                   cdrop=TRUE, na.rm=TRUE, hsep=c('.',':'), vname='variable', vpos=1,  fname='function',
                   fpos=ifelse(is.null(cols), 1, 1+length(cols)), rmissing='Missing', cmissing='Missing',
                   as.data.frame=TRUE, header_minimal=TRUE,  envir=.GlobalEnv, ...) {

  if (is.null(data) || !is.data.frame(data) || nrow(data)==0 || ncol(data)==0) {
    stop("data must be a data.frame with at least one column and one value.")
  }

  # check parameters rmissing and cmissing
  rmissing <- check_characterstring(rmissing, NULL, error_string('rmissing'))
  cmissing <- check_characterstring(cmissing, NULL, error_string('cmissing'))

  # check parameter 'rdrop' and 'cdrop'
  rdrop <- check_logical(rdrop, TRUE, error_string('rdrop'))
  cdrop <- check_logical(cdrop, TRUE, error_string('cdrop'))

  # check parameter 'variable'
  variable <- check_names(variable, names(data), error_string('variable'))
  if (!is.null(variable)) {
    names(variable) <- make.unique(names(variable), sep='_')
  }else {
    stop("The input 'variable' can't be NULL.")
  }

  # check parameter 'rows'and create combined factor for row's of table
  rows <- check_names(rows, names(data), error_string('rows'))
  if (is.null(rows)) {
    row.groups <- interaction_by_numbers(factor(rep(1, nrow(data))))
  } else {
    names(rows) <- make.unique(names(rows), sep='_')
    if (!is.null(rmissing)) {
      data[,rows] <- rapply(data[,rows, drop=FALSE], replacevalues, oldvalues=NA, newvalues=rmissing, warnings=FALSE, droplevels=FALSE, how='replace')
    }
    row.groups <- interaction_by_numbers(data[,rows, drop=FALSE], sep='.', drop=rdrop)
  }

  # check parameter 'cols' and make combined factor for col's per function of table
  cols <- check_names(cols, names(data), error_string('cols'))
  if (is.null(cols)) {
    col.groups <- interaction_by_numbers(factor(rep(1, nrow(data))))
  } else {
    names(cols) <- make.unique(names(cols), sep='_')
    if (!is.null(cmissing)) {
      data[,cols] <- rapply(data[,cols, drop=FALSE], replacevalues, oldvalues=NA, newvalues=cmissing, warnings=FALSE, droplevels=FALSE, how='replace')
    }
    col.groups <- interaction_by_numbers(data[,cols, drop=FALSE], sep='.', drop=cdrop)
  }

  # check position of variable in rows
  vpos <- check_integer(vpos, 1, error_string('vpos'), min=1)

  # check position of function in cols
  fpos <- check_integer(fpos, 1, error_string('fpos'), min=1)

  if (is.null(fun) & is.null(rfun) & is.null(cfun)) {
    fun <- check_function_names(fun, 'count', error_string('fun'), envir=envir)
  } else if (!is.null(fun)) {
    fun <- check_function_names(fun, 'count', error_string('fun'), envir=envir)
  }
  if (!is.null(rfun)) rfun <- check_function_names(rfun, NULL, error_string('rfun'), envir=envir)
  if (!is.null(cfun)) cfun <- check_function_names(cfun, NULL, error_string('cfun'), envir=envir)

  if (TRUE) {
    # check parameter 'fun', 'rfun' and 'cfun' on double names e.g. count in fun and rfun if so change to fun_count and rfun_count
    tab <- table(c(names(fun),names(rfun), names(cfun)))
    tab <- tab[tab>1]
    if (length(tab)>0) {
      if (!is.null(fun)) {
        i <- names(fun) %in% names(tab)
        if  (any(i)) names(fun)[i] <- paste('fun', names(fun)[i], sep='_')
      }
      if (!is.null(rfun)) {
        i <- names(rfun) %in% names(tab)
        if  (any(i)) names(rfun)[i] <- paste('rfun', names(rfun)[i], sep='_')
      }
      if (!is.null(cfun)) {
        i <- names(cfun) %in% names(tab)
        if  (any(i)) names(cfun)[i] <- paste('cfun', names(cfun)[i], sep='_')
      }
    }
    allfun_names <- c(names(fun), names(rfun), names(cfun))
  }

  # create groups, the row key into the table
  if (!is.null(rows)) {
    groups <- lapply(strsplit(levels(row.groups), '.', fixed=TRUE), as.integer)
    groups <- as.data.frame(t(as.matrix(groups)))
    for (i in 1:length(groups)) {
      groups[[i]] <- attr(row.groups, 'levels_i')[[i]][groups[[i]]]
      if (attr(row.groups,'is.fact')[i]) groups[[i]] <- factor(groups[[i]], attr(row.groups, 'levels_i')[[i]])
    }
    names(groups) <- names(rows)

    vars <- factor(rep(names(variable), nrow(groups)), names(variable))
    i <- rep(1:nrow(groups), rep(length(variable), nrow(groups)))
    groups <- cbind(vars, groups[i,,drop=FALSE])
  } else {
    groups=data.frame(factor(names(variable), names(variable)))
  }
  names(groups)[1] <- vname
  ngroups <- nrow(groups)

  # create header, the columns key into the table
  function.name <- fname
  if (!is.null(cols)) {

    header <- lapply(strsplit(levels(col.groups), '.', fixed=TRUE), as.integer)
    header <- as.data.frame(t(as.matrix(header)))
    for (i in 1:length(header)) {
      header[[i]] <- attr(col.groups, 'levels_i')[[i]][header[[i]]]
      if (attr(col.groups,'is.fact')[i]) header[[i]] <- factor(header[[i]], attr(col.groups, 'levels_i')[[i]])
    }
    names(header) <- names(cols)

    if (function.name %in% names(header)) {
      i <- 2; while ((function.name<-paste0('function_',i)) %in% names(header)) { i <- i+1 }
    }
    tempvar <- factor(rep(allfun_names, rep(nrow(header), length(allfun_names))), allfun_names)
    header <- cbind(tempvar, header)
  } else {
    header <- data.frame(factor(allfun_names, allfun_names))
  }
  names(header)[1] <- function.name
  nHeader <- nrow(header)

  # combine rows and columns into one factor and split data accordingly
  all.groups <- interaction(row.groups, col.groups, sep='.', drop=FALSE)
  data_ij <- split(data[,variable, drop=FALSE], all.groups, drop=FALSE)


  c_out <- r_out <- out <- NULL
  if (!is.null(fun)) {
    # evaluate per function all variables and store each value in a list element
    out <- lapply(names(fun), FUN=function(f) { ctable_rapply_function(data_ij, fun[[f]], f, na.rm=na.rm, ...) })
    f <- names(fun)
    # out <- lapply(names(fun), FUN=function(f) { ctable_rapply_function(data_ij, fun[[f]], f, na.rm=na.rm) })
    if (any(rapply(out, length, how='unlist')!=1)) {
      stop(sprintf("Error in evaluating functions, one of the functions generates more than on output value"))
    }
  }


  if (!is.null(rfun)) {
    data_i <- split(data[,variable, drop=FALSE], row.groups, drop=FALSE)
    r_out <- lapply(names(rfun), function(f) { ctable_mapply_function(rfun[[f]], f, data_ij, data_i, na.rm=na.rm, ...) })
    #  r_out <- lapply(names(rfun), function(f) { ctable_mapply_function(rfun[[f]], f, data_ij, data_i, na.rm=na.rm) }) # for interactive debugging
    if (any(rapply(r_out, length, how='unlist')!=1)) {
      stop(sprintf("Error in evaluating functions, one of the rfun functions generates more than on output value"))
    }
  }

  if (!is.null(cfun)) {
    data_j <- split(data[,variable, drop=FALSE], col.groups, drop=FALSE)
    nrows <- nlevels(row.groups)
    ncols <- nlevels(col.groups)
    #   i <- rep(seq_len(nrows), rep(ncols, nrows))
    i <- rep(seq_len(ncols), rep(nrows, ncols))
    c_out <- lapply(names(cfun), function(f) { ctable_mapply_function(cfun[[f]], f, data_ij, data_j[i], na.rm=na.rm, ...)})
    # c_out <- lapply(names(cfun), function(f) { ctable_mapply_function(cfun[[f]], f, data_ij, data_j[i], na.rm=na.rm)})
  }

  out <- c(out, r_out, c_out)

  # set results of each fun in a separate matrix and transform to data.frame (results can have different types)
  list_of_dataframes <- lapply(out, FUN=function(x) { x <- unlist(x);
  as.data.frame(matrix(x, nrow=ngroups, ncol=length(x)/ngroups))})

  if (!all(sapply(list_of_dataframes, nrow) == nrow(list_of_dataframes[]))) {
    stop(sprintf("Error in evaluating functions, not all result matrices have equal number of rows"))
  }
  result <- do.call(cbind, list_of_dataframes)

  fpos <- min(fpos, ncol(header))
  if (fpos!=1) {
    if (fpos==ncol(header)) {
      header <- header[,c(seq(2, ncol(header)), 1), drop=FALSE]
    } else {
      seq <- c(seq(2, fpos), 1, seq(min(fpos+1, ncol(header)), ncol(header)))
      header <- header[,seq, drop=FALSE]
    }
  }
  sorted_col_index <- do.call(order, header)
  header <- header[sorted_col_index, , drop=FALSE]

  vpos <- min(vpos, ncol(groups))
  if (vpos!=1) {
    if (vpos==ncol(groups)) {
      groups <- groups[,c(seq(2,ncol(groups)), 1), drop=FALSE]
    } else {
      seq <- c(seq(2, vpos),1, seq(min(vpos+1,ncol(groups)),ncol(groups)))
      groups <- groups[,seq, drop=FALSE]
    }
  }
  sorted_row_index <- do.call(order, groups)
  groups <- groups[sorted_row_index, , drop=FALSE]

  result <- result[sorted_row_index, sorted_col_index, drop=FALSE]
  names(result) <- header_to_column_names(header, sep_col_value=hsep[1], sep_col_value_pair=hsep[2], header_minimal=FALSE)
  table <- cbind(groups,result)

  header <- rapply(header, function(x) {levels(x)[x]}, classes='factor', how='replace')
  attr(table, 'cols') <- header
  class(attr(table, 'cols')) <- c('ctable_cols', class(attr(table, 'cols')))

  attr(table, 'rows') <- groups
  class(attr(table, 'rows')) <- c('ctable_rows', class(attr(table, 'rows')))

  attr(table, 'vname') <- vname
  class(attr(table, 'vname')) <- c('ctable_vname', class(attr(table, 'vname')))

  attr(table, 'fname') <- fname
  class(attr(table, 'fname')) <- c('ctable_fname', class(attr(table, 'fname')))

  attr(table, 'hsep') <- hsep
  class(attr(table, 'hsep')) <- c('ctable_hsep', class(attr(table, 'hsep')))

  class(table) <- c('ctable', class(table))
  class(attr(table, 'names')) <- c('ctable_names', class(attr(table, 'names')))

  t <- table(names(table))
  if (any(t>1)) {
    warning('columns with the same header found for ',paste0("(", paste(names(t)[t>1], collapse=', '),"); rename with named variable"))
  }


  if (as.data.frame) {
    table <- as.data.frame(table, header_minimal=header_minimal)
  }

  return(table)
}

#' is.ctable
#' checks if the structure has 'ctable' in its class definition
#'
#' @param x input data structure
#'
#' @returns TRUE or FALSE
#' @export
#'
#' @examples
#' is.ctable(tab)
is.ctable <- function(x) {
  'ctable' %in% class(x)
}

#' is_valid_ctable
#' checks if the structure is a valid ctable i.e. contains all the nescessary attributes
#'
#' @param x input data structure
#' @param messages if TRUE messages will be printed otherwise execution will stop
#'
#' @returns TRUE or FALSE
#' @export
#'
#' @examples
#' is_valid_ctable(tab)
is_valid_ctable <-function(x, messages=TRUE) {
  if (isTRUE(messages)) {
    fun <- base::message
  } else {
    fun <- base::stop
  }
  if (is.null(x) || !is.ctable(x)) {
    fun(sprintf('is_valid_ctable: argument x is not a ctable'))
    return(FALSE)
  }
  attributes <- c("names", "class", "row.names", "cols", "rows", "vname", "fname", "hsep")
  missing_attributes <- attributes[!(attributes %in% names(attributes(x)))]
  if (length(missing_attributes)>0) {
    fun(sprintf("attributes: %s not found",
                paste(sprintf("'%s'", missing_attributes), collapse = ", ")))
    return(FALSE)
  }

  # check each attribute
  rows <- rows(x)
  if (!is.data.frame(rows)) {
    fun(sprintf("The rows attribute does not contain a data.frame"))
  } else {
    if (nrow(rows)!=nrow(x)) {
      sprintf("the number of rows in rows(x) must be the same as the number of rows in x")
      return(FALSE)
    }

    cond <- names(rows) %in% names(x)
    if (!all(cond)) {
      d <- duplicated(rows[,cond, drop=FALSE]) # must be a key
      if (sum(d==FALSE)!=nrow(rows)) {
        fun(sprintf("The columns %s in the table do not define a key", paste(names(rows)[cond], collapse=', ')))
        return(FALSE)
      }

      if (!all(sapply(rows[,!cond, drop=FALSE], function(x) { all(x==x[1]) }))) {
        fun(sprintf("The variables %s are not all each unique", paste(names(rows)[!cond], collapse=", ") ))
        return(FALSE)
      }
    }
  }

  hsep <- attr(x, 'hsep')
  if (!((is.null(hsep) || !is.character(hsep) || (2 != length(hsep)) || all(nchar(hsep)==1)))) {
    fun(sprintf("the attribute 'hsep' must be a character vector of length 2 each having one character"))
    return(FALSE)
  }

  names_other <- names(x)[!(names(x) %in% names(rows(x)))]

  header <- cols(x)
  header_minimal <- header_to_column_names(header, hsep[1], hsep[2], header_minimal=TRUE)
  header_maximal <- header_to_column_names(header, hsep[1], hsep[2], header_minimal=FALSE)
  if (!(all(header_minimal %in% names_other) || all(header_maximal %in% names_other))) {
    fun(sprintf("The cols(x) contains extra information, which is not in the names(x)"))
    return(FALSE)
  }
  if (!(all(names_other %in% header_minimal) || all(names_other %in% header_maximal))) {
    fun(sprintf('extra columns found not defined in rows(x) or cols(x)'))
    return(FALSE)
  }
  names(attributes(x))

  vname <- attr(x, 'vname')
  if (is.null(vname) || !is.character(vname) || 1!=length(vname) || !(vname %in% names(rows(x)))) {
    fun(sprintf("the vname attribute must be a character string of length 1 and must be a name in rows(x)"))
    return(FALSE)
  }

  fname <- attr(x, 'fname')
  if (is.null(fname) || !is.character(fname) || 1!=length(fname) || !(fname %in% names(cols(x)))) {
    fun(sprintf("the fname attribute must be a character string of length 1 and must be a name in cols(x)"))
    return(FALSE)
  }

  return(TRUE)
}

#' as.matrix.ctable
#'
#' @param x
#' @param sep
#' @param ...
#'
#' @returns
#' @export
#'
#' @examples
as.matrix.ctable <- function(x, sep='.', ...) {
  fx <- as.data.frame(x)
  hsep <- hsep(x)
  res <- as.matrix(fx[,!(names(fx) %in% names(rows(x)))])
  row.names(res) <- header_to_column_names(rows(x), hsep[1], hsep[2], header_minimal=FALSE)
  res
}

#' as.data.frame.ctable
#'
#' @param table
#' @param col.names
#' @param header_minimal
#' @param ...
#'
#' @returns
#' @export
#'
#' @examples
as.data.frame.ctable <- function(table, col.names=NULL, header_minimal=TRUE, ...) {
  if (is_valid_ctable(table, message=FALSE)) {
    hsep <- attr(table, 'hsep')
    c <- cols(table)
    col.names <- c(names(rows(table)), header_to_column_names(c, first(hsep),
                                                              last(hsep), header_minimal=header_minimal))
  } else {
    col.names <- attr(table, 'names')
  }
  class(table) <- class(table)[class(table) != 'ctable']
  names(table) <- col.names
  as.data.frame(table, ...)
}
