#' @noRd
#' helper function in ctable and get_default_args
replace_args <- function(f, fname=NULL, ...) {

  if (is.function(f)) {
    if (is.null(fname)) fname <- as.character(quote(f))
    f_args <- as.list(formals(args(f))) # works for primitives and closures
  } else if (is.character(f)) {
    fname <- f
    func <- tryCatch(if (is.character(substitute(f))) {
      get(f, mode = "function", envir = parent.frame())
    } else {
      print(class(f))
      eval(substitute(f), parent.frame())
    }, error = function(e) stop("Invalid function: ", e$message))
    f_args <- as.list(formals(args(func)))
  } else if (is.list(f)) {
    f_args <- f
  }
  args <- list(...)

  # Directly replace matching arguments using subset assignment
  common_args <- intersect(names(args), names(f_args))
  f_args[common_args] <- args[common_args]

  if (!is.null(fname)) {
    specific_arg_names <- names(args)[grepl(paste0('^',fname,'_'), names(args))]
    specific_args <- intersect(names(f_args), sub(paste0('^', fname, '_'), '', specific_arg_names))
    f_args[specific_args] <- args[paste0(fname,'_',specific_args)]
  }

  f_args
}



#' get_default_args
#'
#' @description
#' Evaluate Default Arguments of a Function
#'
#' Retrieves and evaluates the default values for all arguments in a function,
#' including dynamically computed defaults (like `Sys.Date()` or expressions).
#'
#' @param func_name Character string of the function name, or an unquoted function.
#' @param eval_env Environment in which to evaluate defaults. Defaults to the
#'                 function's own environment.
#'
#' @return A named list of evaluated default values (arguments without defaults are omitted).
#'
#' @examples
#' # Function with static and dynamic defaults
#' myfunc <- function(a = 2^3, b = Sys.Date(), c) { a + as.numeric(b) }
#'
#' # Get evaluated defaults
#' get_default_args(myfunc)  # Returns list(a = 8, b = <current date>)
#'
#' @export
get_default_args <- function (func_name, ..., eval_env = parent.frame()) {
  if (!is.environment(eval_env)) stop("eval_env must be an environment")

  func <- tryCatch(if (is.character(substitute(func_name))) {
    get(func_name, mode = "function", envir = parent.frame())
  } else {
    eval(substitute(func_name), parent.frame())
  }, error = function(e) stop("Invalid function: ", e$message))

  f_args <- replace_args(f=func, fname=NULL, ...)

  res <- setNames(rep("Not Set", length(f_args)), names(f_args))
  has_default <- !sapply(f_args, rlang::is_missing)

  if (any(has_default)) {
    res[has_default] <- "Set"
    for (arg in names(f_args)[has_default]) {
      tryCatch({
        assign(arg, eval(f_args[[arg]], eval_env), eval_env)
      }, error = function(e) {
        res[arg] <<- "Error"
      })
    }
  }
  return(res)
}



#' remove_default_args
#'
#' @param func_name
#' @param eval_env
#'
#' @return
#' @export
#'
#' @examples
remove_default_args <- function(func_name, eval_env = parent.frame()) {

  # Validate environment
  if (!is.environment(eval_env)) stop("eval_env must be an environment")

  # Get function with error handling
  func <- tryCatch(
    if (is.character(substitute(func_name))) {
      get(func_name, mode = "function", envir = parent.frame())
    } else {
      eval(substitute(func_name), parent.frame())
    },
    error = function(e) stop("Invalid function: ", e$message)
  )

  f_args <- formals(func)
  vars_to_remove <- names(f_args)[!sapply(f_args, rlang::is_missing)]

  # Filter to existing variables
  existing_vars <- vars_to_remove[vars_to_remove %in% ls(envir = eval_env)]

  if (length(existing_vars) > 0) {
    rm(list = existing_vars, envir = eval_env)
  }

  invisible(existing_vars)
}
