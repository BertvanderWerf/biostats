#' Calculate Cumulative Sums with Optional Grouping, Ordering, and Scaling
#'
#' This function computes the cumulative sum for a vector or a column in a data frame,
#' supporting custom ordering, grouping, and optional scaling to [0, 1].
#'
#' @param x Numeric vector or data frame. If a data frame, columns can be specified.
#' @param cum_var Character. Column name for values to cumulatively sum. Required for data frames.
#' @param order_by Character. Column name to order within groups or data. Optional.
#' @param group_vars Character vector. Column names for grouping variables. Optional.
#' @param scale Logical. Should result be scaled to max in each group? Default is TRUE.
#' @param na_rm Logical. Should missing values be ignored? Default is TRUE.
#'
#' @return Numeric vector of cumulative sums.
#' @examples
#' CumSum(1:5)
#' df <- data.frame(a = c(1,2,NA,4), grp = c("A", "A", "B", "B"))
#' CumSum(df, cum_var = "a", group_vars = "grp")
#' @export
CumSum <- function(x, cum_var = NULL, order_by = NULL, group_vars = NULL, scale = TRUE, na_rm = TRUE) {
  # Internal function to compute cumsum on subset
  cumsum_subset <- function(df, cum_var, order_by, scale, na_rm) {
    idx <- if (!is.null(order_by)) order(df[[order_by]]) else seq_len(nrow(df))
    values <- df[[cum_var]][idx]
    na_mask <- is.na(values)
    values[na_mask & na_rm] <- 0
    cs <- cumsum(values)
    cs[na_mask & na_rm] <- NA
    if (scale) cs <- cs / max(cs, na.rm = TRUE)
    out <- cs[order(idx)]
    return(out)
  }

  # Helper to generate a temporary column name
  tempname <- function(existing_names) {
    base <- ".temp"
    nm <- base
    i <- 1
    while (nm %in% existing_names) {
      nm <- paste0(base, i)
      i <- i + 1
    }
    nm
  }

  # Input validation
  #  if (is.null(cum_var) && is.data.frame(x)) stop("cum_var must be specified for data frames")
  if (!is.null(cum_var) && !cum_var %in% names(x)) stop("cum_var does not exist in data")
  if (!is.null(order_by) && !order_by %in% names(x)) stop("order_by does not exist in data")
  if (!is.null(group_vars) && !all(group_vars %in% names(x))) stop("Some group_vars do not exist in data")

  # Main logic
  if (!is.data.frame(x)) {
    # Vector case
    data <- x
    na_mask <- is.na(data)
    data[na_mask & na_rm] <- 0
    cs <- cumsum(data)
    cs[na_mask & na_rm] <- NA
    if (scale) cs <- cs / max(cs, na.rm = TRUE)
    return(cs)
  } else {
    df <- x
    # Add order_by if missing
    if (is.null(order_by)) {
      order_by <- tempname(names(df))
      df[[order_by]] <- seq_len(nrow(df))
    }
    # Add cum_var if missing (rare)
    if (is.null(cum_var)) {
      cum_var <- tempname(names(df))
      df[[cum_var]] <- 1
    }
    # No groups
    if (is.null(group_vars)) {
      return(cumsum_subset(df, cum_var, order_by, scale, na_rm))
    } else {
      f <- interaction(df[group_vars], drop = TRUE)
      result <- unsplit(
        lapply(split(df, f),
               function(sub_df) {
                 cumsum_subset(sub_df, cum_var, order_by, scale, na_rm)
               }
        ),
        f
      )
      return(result)
    }
  }
}
