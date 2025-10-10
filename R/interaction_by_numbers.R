#' interaction_by_numbers
#'
#' @description
#'
#' Create Interaction Factor from Numeric Level Representations
#'
#' Similar to [interaction()] but creates combinations based on factor level
#' numbers rather than actual level values. Preserves original factor metadata.
#'
#' @param ... One or more vectors/factors (will be converted to factors if needed)
#' @param drop Logical indicating whether to drop unused levels (default: FALSE)
#' @param sep Character string to separate levels (default: ".")
#' @param lex.order Logical indicating whether to use lexical ordering (default: FALSE)
#'
#' @return A factor representing interactions with attributes:
#' \itemize{
#'   \item{is.fact: Logical vector indicating original factor status of inputs}
#'   \item{levels_i: List of original factor levels for each input}
#' }
#'
#' @examples
#' # Basic usage
#' f1 <- factor(c("a", "b", "a"))
#' f2 <- factor(c("x", "x", "y"))
#' int_factors <- interaction_by_numbers(f1, f2)
#' levels(int_factors)  # "1.1" "2.1" "1.2"
#' attr(int_factors, "levels_i")  # List of original levels
#'
#' # With character vectors
#' chr_vec <- c("high", "low", "high")
#' mixed_int <- interaction_by_numbers(f1, chr_vec)
#'
#' @export
interaction_by_numbers <- function(..., drop = FALSE, sep = ".", lex.order = FALSE) {
  # Convert all inputs to factors
  args <- list(...)
  narg <- length(args)
  if (narg < 1)
    stop("No factors specified")
  if (narg==1 && is.list(args[[1]])) {
    args <- args[[1]]
    narg <- length(args)
  }
  is.fact <- vapply(args, is.factor, logical(1))
  levels <- list()
  for (i in 1:narg) {
    if (is.fact[i]) {
      levels[[i]] <- levels(args[[i]])
    } else {
      levels[[i]] <- sort(unique(args[[i]])) # preserve type
      args[[i]] <- factor(args[[i]], levels[[i]])
    }
  }
  facts_i <- lapply(args, function(x) factor(as.integer(x)))
  # Create interaction
  result <- interaction(facts_i, drop = drop, sep = sep, lex.order = lex.order)
  # Add metadata attributes
  attr(result, "is.fact") <- is.fact
  attr(result, "levels_i") <- levels
  return(result)
}
