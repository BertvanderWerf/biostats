
#' @exportS3Method sandwich::estfun
estfun.gnls <- function(x, Adjust = TRUE, ...) {
  if (!inherits(x, "gnls")) stop("Works for gnls only")
  fit <- x

  # 1. Extract parameter vectors
  pars <- lapply(fit$pmap, function(index) coefficients(fit)[index])
  pars <- mapply(function(x, y) c(x %*% y), fit$plist, pars, SIMPLIFY = FALSE, USE.NAMES = TRUE)

  # 2. Get formula variables
  f <- formula(fit)
  vars <- all.vars(extractFixedFormula(f))
  vars <- vars[!vars %in% names(pars)]

  data <- nlme::getData(fit)

  # 3. Create evaluation environment
  eval_env <- new.env(parent = baseenv())

  # Assign data variables within evaluation environment
  for (var in vars) {
    eval_env[[var]] <- data[[var]]
  }
  # Assign parameter vectors
  for (i in seq_along(pars)) {
    eval_env[[names(pars)[i]]] <- pars[[i]]
  }

  # 4. Derivative and evaluation
  derivs <- deriv(f, names(pars))
  fitted <- eval(derivs, envir = eval_env)
  gradient <- attr(fitted, 'gradient')
  gradient <- mapply(function(x, mat) x * mat, as.data.frame(gradient), fit$plist, SIMPLIFY = FALSE, USE.NAMES = TRUE)
  gradient <- do.call(cbind, gradient)
  colnames(gradient) <- names(coefficients(fit))

  # 5. Residuals and variance
  r <- residuals(fit)
  n <- length(r)
  np <- length(coefficients(fit))
  s2 <- if (Adjust) sum(r^2, na.rm = TRUE) / (n - np) else sum(r^2, na.rm = TRUE) / n

  # 6. Return score matrix
  gradient * r / s2
}
