#' lof_test
#' Lack of Fit Test for Regression Models
#'
#' Performs a lack of fit test by partitioning the residual sum of squares into
#' pure error and lack of fit components.
#'
#' @param fitted Numeric vector of fitted values from the model.
#' @param residuals Numeric vector of residuals from the model.
#' @param npar Integer, number of parameters in the model (including intercept).
#' @param error_factor Optional factor indicating replicate groups for pure error estimation.
#'   If NULL, groups are defined by unique fitted values.
#'
#' @return An anova like object (data.frame) with components:
#'   \describe{
#'     \item{SS}{Sum of squares}
#'     \item{df}{Degrees of freedom}
#'     \item{F}{F statistic}
#'     \item{p.value}{P-value for the lack of fit test}
#'   }
#' @examples
#' fit <- lm(y ~ x, data = some_data)
#' lof_test(fitted(fit), residuals(fit), npar = length(coef(fit)),
#'          error_factor = interaction(some_data$group1, some_data$group2))
#' @export
lof_test <- function(fitted, residuals, npar, error_factor=NULL) {
  # Create error_factor if not provided
  if (is.null(error_factor)) {
    error_factor <- factor(fitted)
  } else {
    stopifnot(is.factor(error_factor))
  }

  # observed values
  y <- fitted + residuals

  # Group means for each replicate group
  group_means <- tapply(y, error_factor, mean, na.rm = TRUE)
  m <- group_means[as.integer(error_factor)]
  n_groups <- nlevels(error_factor)
  N <- length(error_factor)

  SS <- function(x) sum(x*x, na.rm=TRUE)

  # Pure error: variation within replicate groups
  SS_pure_error <- SS(y - m)
  df_pure_error <- N - n_groups
  MS_pure_error  <- SS_pure_error / df_pure_error

  # alternative way to calculate the SS_lof, but it can be negative!
  SS_error <- SS(y - fitted)
  SS_lof1 <- SS_error - SS_pure_error
  if (SS_lof1<0) {
    Warning('SS_lof is negative if calculated as a difference between SS_error and SS_pur_error, check inputs')
  }

  # Compute lack of fit sum of squares (between-group variation)
  SS_lof <- SS(m - fitted)
  df_lof <- n_groups - npar
  MS_lof <- SS_lof / df_lof

  # F statistic and p-value
  F_lof <- MS_lof / MS_pure_error
  pval <- pf(F_lof, df_lof, df_pure_error, lower.tail = FALSE)

  result <- data.frame('Source of Variation' = c('Lack of Fit', 'Pure Error'),
                       SS = c(SS_lof, SS_pure_error),
                       df = c(df_lof, df_pure_error),
                       MS = c(MS_lof, MS_pure_error),
                       F  = c(F_lof, NA),
                       p_value = c(pval, NA))
  return(result)
}

# lof_test(fitted(fit), residuals(fit), npar=length(coefficients(fit)),
#          error_factor=interaction(data$Time.point, data$Cell.line, data$Treatment))
#
# lof_test(fitted(fit), residuals(fit), npar=length(coefficients(fit)))

