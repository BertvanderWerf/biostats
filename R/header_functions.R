#' Check if Input is a Single Character
#'
#' This function checks whether the provided input is a single character.
#'
#' @param input The input to be checked.
#'
#' @return A logical value indicating whether the input is a single character (`TRUE`) or not (`FALSE`).
#'
#' @examples
#' is.single_char("a")    # Returns TRUE
#' is.single_char("ab")   # Returns FALSE
#' is.single_char(1)      # Returns FALSE
#' is.single_char(NULL)   # Returns FALSE
#'
#' @export
is.single_char <- function(input) {
  # Check if input is not NULL, is a character, has length 1, and is exactly one character long
  !is.null(input) &&
    is.character(input) &&
    length(input) == 1 &&
    nchar(input) == 1
}

#' Convert a Data Frame Header to Column Names
#'
#' This function takes a data frame and converts its header (column names and values)
#' into a single character vector of concatenated column names.
#'
#' @param header A data frame whose column names and values will be converted.
#' @param sep_col_value A character string used to separate column names from their values.
#' @param sep_col_value_pair A character string used to separate each column name-value pair.
#' @param header_minimal Logical controlling header simplification (default: TRUE)
#' @param the name of the function column in the header data frame (default function)
#'
#' @return A character vector containing concatenated column names in the format
#'         "ColumnName:Value".
#'
#' @examples
#' header <- data.frame(A = c("a1", "a2"), B = c("b1", "b2"), C = c("c1", "c2"))
#' concatenated <- header_to_column_names(header)
#'
#' @export
header_to_column_names <- function(header, sep_col_value='.', sep_col_value_pair=':', header_minimal=TRUE, fname='function') {

  # Check for valid separators using the helper function
  if (!is.single_char(sep_col_value)) {
    stop("sep_col_value must be a single character.")
  }
  if (!is.single_char(sep_col_value_pair)) {
    stop("sep_col_value_pair must be a single character.")
  }

  if (is.null(header) || !is.data.frame(header) || nrow(header)==0 || ncol(header)==0) {
    stop("Header must be a data.frame with at least one column and one value.")
  }

  # Check for invalid characters in header values
  if (any(grepl(sep_col_value, unlist(header), fixed=TRUE) |
          grepl(sep_col_value_pair, unlist(header), fixed=TRUE))) {
    stop("Header values must not contain the specified separators.")
  }

  if (!header_minimal) {
    names <- do.call(paste, c(as.data.frame(mapply(paste, names(header), header, sep=sep_col_value, SIMPLIFY = FALSE)), sep=sep_col_value_pair))
  } else {
    # if all values in column are equal, do not print column
    # do not print columnnames
    if (nrow(header)==1) {
      if (ncol(header)>1) { # drop function name

      }
    }
    cond <- !sapply(header, function(x) { if (length(x)==1) FALSE else all(x==x[1])})
    if (any(cond)) {
      header_minimal <- header[,cond, drop=FALSE]
      names <- do.call(paste, c(header_minimal, sep=sep_col_value_pair))
    } else {
      names <- do.call(paste, c(header, sep=sep_col_value_pair))
    }
  }
  return(names)
}

#' Convert Column Names Back to a Data Frame Header
#'
#' This function takes a character vector of concatenated column names and converts it back
#' into a data frame with appropriate headers.
#'
#' @param column_names A character vector containing concatenated column names in the format
#'                     "ColumnName:Value".
#' @param sep_col_value A character string used to separate column names from their values.
#' @param sep_col_value_pair A character string used to separate each column name-value pair.
#'
#' @return A data frame reconstructed from the provided column names.
#'
#' @examples
#' concatenated <- header_to_column_names(header)
#' header_reconstructed <- column_names_to_header(concatenated)
#'
#' @export
column_names_to_header <- function(column_names, sep_col_value='.', sep_col_value_pair=':') {
  # Check for valid separators using the helper function
  if (!is.single_char(sep_col_value)) {
    stop("sep_col_value must be a single character.")
  }
  if (!is.single_char(sep_col_value_pair)) {
    stop("sep_col_value_pair must be a single character.")
  }

  if (is.null(column_names) || !is.character(column_names) || length(column_names)==0 ||
      any(nchar(column_names)==0)) {
    stop("Input must contain strings with valid column name-value pairs.")
  }

  # Reverse the operation
  split_data <- strsplit(column_names, sep_col_value_pair, fixed=TRUE)
  count_col_value_pair <- sapply(split_data, length)
  if (any(count_col_value_pair != count_col_value_pair[1])) {
    stop("Each input item should contain equal number of column-value pairs.")
  }

  split_data <- do.call(rbind, split_data) #strsplit(column_names, sep_col_value_pair, fixed=TRUE))

  count_sep_col_values <- lapply(split_data, FUN=function(x) { lengths(regmatches(x, gregexpr(sep_col_value, x, fixed = TRUE)))})
  if (any(count_sep_col_values!=1)) {
    stop("Input must contain valid column name-value pairs.")
  }

  # Split each entry into column names and values
  result_df <- lapply(split_data, function(x) {
    do.call(rbind, strsplit(x, sep_col_value, fixed=TRUE))
  })
  count_pair_items <- lapply(result_df, length)
  if (any(count_pair_items != 2)) {
    stop("Input must contain valid column name-value pairs")
  }
  result_df <- do.call(rbind, result_df)

  # Convert to a data frame with appropriate column names
  result_df <- as.data.frame(result_df)
  names(result_df) <- c("Column_Name", "Value")

  header_reconstructed <- split(result_df$Value, result_df$Column_Name)
  header_reconstructed <- as.data.frame(do.call(cbind, header_reconstructed))

  return(header_reconstructed)
}
