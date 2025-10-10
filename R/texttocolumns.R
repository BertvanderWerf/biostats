#' TextToColumns
#' Convert Delimited Text into a DataFrame with Numeric Columns
#'
#' Splits each string in a character vector into indicator columns as a dataframe.
#' Item and missing columns are numeric (0/1), "Other" column is character.
#'
#' @param x Character vector to split and analyze.
#' @param sep Character delimiter for splitting items (default: ';').
#' @param items Optional character vector of item names to define output columns. If provided, all non-matching items are collapsed into an "Other" column.
#' @param other_name Name for the "Other" column (default: "Other").
#' @param other_sep String to use to collapse multiple "Other" entries (default: '|').
#' @param NA_col Name for the missing-value indicator column. If NULL, skip; if a string, column name can be set (e.g. NA_col="Missing").
#' @param ignore.case Logical; if TRUE, ignores case (default: FALSE).
#'
#' @return A dataframe with columns for specified items, "Other", and missing-indicator as needed. Numeric columns whenever possible.
#' @export
TextToColumns <- function(x, sep=';', items=NULL, other_name="Other", other_sep="|",
                          NA_col=NULL, ignore.case=FALSE) {
  # Helper function to trim whitespace from start/end of string
  trim <- function(x) gsub("^\\s+|\\s+$", "", x)

  # Identify missing entries; treat NAs as missing and empty strings
  is_missing <- is.na(x)
  x_trimmed <- ifelse(is_missing, "", trim(as.character(x)))
  # Apply case folding if requested
  x_main <- if (ignore.case) tolower(x_trimmed) else x_trimmed

  # Split each entry into a character vector of items
  split_list <- strsplit(x_main, sep, fixed=TRUE)
  split_list <- lapply(split_list, trim)
  # For missing entries, assign an empty character vector
  split_list[is_missing] <- list(character(0))

  # Compile unique items from all rows (excluding empty strings)
  all_unique <- sort(unique(unlist(split_list)))
  all_unique <- all_unique[all_unique != ""]

  # Decide which columns to use: if 'items' specified, handle "Other"; else use all unique items
  if (is.null(items)) {
    item_set <- all_unique
    use_other <- FALSE
  } else {
    item_set <- if (ignore.case) tolower(trim(items)) else trim(items)
    use_other <- TRUE
  }

  # Create an empty list to build the data.frame columns explicitly as correct types
  out <- vector("list", length = length(item_set) + as.integer(use_other) + as.integer(!is.null(NA_col)))
  names(out) <- c(item_set,
                  if (use_other) other_name,
                  if (!is.null(NA_col)) as.character(NA_col))

  # Fill item columns as numeric 0/1
  for (j in seq_along(item_set)) {
    out[[j]] <- as.numeric(vapply(split_list, function(row) item_set[j] %in% row, integer(1)))
  }

  next_col <- length(item_set) + 1

  # Fill the "Other" column, collapsing extra items per row using other_sep; NA if no others
  if (use_other) {
    out[[other_name]] <- vapply(split_list, function(row) {
      others <- setdiff(row, item_set)
      if (length(others)) paste(others, collapse=other_sep) else NA_character_
    }, character(1))
  }

  # Fill the missing indicator column as numeric 0/1 if requested
  if (!is.null(NA_col)) {
    out[[as.character(NA_col)]] <- as.numeric(is_missing)
  }

  # Combine into a data.frame
  df <- as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)

  # Ensure all but "Other" are numeric (helps if there was any unexpected coercion)
  for (nm in names(df)) {
    if (nm != other_name) {
      df[[nm]] <- as.numeric(df[[nm]])
    }
  }

  # Return the final, correctly-typed dataframe
  return(df)
}

