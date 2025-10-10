#' Tukey-Kramer
#' @title Tukey-Kramer Pairwise Comparisons
#' @description Calculates confidence intervals and p-values for all pairwise comparisons
#' between group means using the Tukey-Kramer method. Appropriate for groups with
#' unequal sample sizes but assumes homogeneity of variances.
#'
#' @param means Numeric vector of group means (length k)
#' @param vars Numeric vector of group variances (length k)
#' @param ni Numeric vector of group sample sizes (length k)
#' @param alpha Significance level (default = 0.05)
#'
#' @return A list containing four k x k matrices:
#' \item{differences}{Matrix of mean differences (row - column)}
#' \item{lower}{Lower bounds of confidence intervals}
#' \item{upper}{Upper bounds of confidence intervals}
#' \item{p_values}{Adjusted p-values for pairwise comparisons}
#'
#' @details
#' This implementation:
#' \itemize{
#'   \item{Calculates pooled variance using all groups}
#'   \item{Uses studentized range distribution for adjustment}
#'   \item{Handles NA values by exclusion in calculations}
#' }
#'
#' @examples
#' # Three groups with unequal sample sizes
#' means <- c(10.2, 12.8, 9.5)
#' vars <- c(2.3, 3.1, 2.8)
#' ni <- c(10, 15, 12)
#'
#' result <- Tukey_Kramer(means, vars, ni)
#' print(result$p_values < 0.05)  # Significant comparisons
#'
#' @export
Tukey_Kramer <- function(means, vars, ni, alpha = 0.05) {
  # Input validation
  if (!all.equal(length(means), length(vars), length(ni))) {
    stop("means, vars, and ni must be vectors of equal length")
  }

  # Calculate pooled variance (assuming homogeneity)
  valid <- !is.na(means) & !is.na(vars) & !is.na(ni)
  df_pooled <- sum((ni[valid] - 1) * vars[valid], na.rm = TRUE) /
    sum(ni[valid] - 1, na.rm = TRUE)
  sigma <- sqrt(df_pooled)

  # Create pairwise comparison matrices
  mean_diff <- outer(means, means, '-')
  se_matrix <- sigma / sqrt(2) * sqrt(outer(1/ni, 1/ni, '+'))

  # Calculate critical value
  k <- sum(valid)  # Number of valid groups
  N <- sum(ni[valid], na.rm = TRUE)
  q_crit <- stats::qtukey(p = 1 - alpha, nmeans = k, df = N - k)

  # Calculate confidence intervals
  lower <- mean_diff - q_crit * se_matrix
  upper <- mean_diff + q_crit * se_matrix

  # Calculate p-values using studentized range distribution
  test_stats <- abs(mean_diff) / se_matrix
  p_vals <- 1 - stats::ptukey(test_stats, nmeans = k, df = N - k)

  return(list(
    differences = mean_diff,
    lower = lower,
    upper = upper,
    p_values = p_vals
  ))
}

#' Tukey_Kramer_cov
#' @title Multivariate Tukey-Kramer Pairwise Comparisons
#' @description Computes simultaneous confidence intervals and p-values for all pairwise comparisons among group means using the covariance matrix of the means, following the multivariate Tukey-Kramer approach.
#'
#' @param means Numeric vector of group means (length k)
#' @param covmat Covariance matrix of the group means (k x k)
#' @param alpha Significance level (default = 0.05)
#'
#' @return A list containing:
#' \item{differences}{Matrix of mean differences (row - column)}
#' \item{lower}{Lower bounds of confidence intervals}
#' \item{upper}{Upper bounds of confidence intervals}
#' \item{test_stat}{Matrix of test statistics for each pair}
#' \item{p_values}{Approximate p-values for pairwise comparisons}
#' \item{q_crit}{the q criterium for the Tukey-Kramer statistic}
#'
#' @details
#' This function uses the covariance matrix of the group means to calculate the standard error for each pairwise difference. The critical value is based on the studentized range distribution, but for correlated means, the approximation is conservative[1][5]. For small numbers of groups, this approach is particularly robust.
#'
#' The Multivariate Tukey-Kramer Multiple Comparison Procedure Among Four Correlated Mean Vectors
#' Takashi Seo  & Takahiro Nishiyama
#'
#' On the conservative multivariate Tukey-Kramer type procedures for multiple comparisons among mean vectors
#' Takashi Seo & Takahiro Nishiyama
#'
#' @examples
#' means <- c(10.2, 12.8, 9.5)
#' covmat <- matrix(c(0.23, 0.05, 0.04,
#'                   0.05, 0.31, 0.06,
#'                   0.04, 0.06, 0.28), 3, 3)
#' Tukey_Kramer_cov(means, covmat)
#'
#' @export
Tukey_Kramer_cov <- function(means, covmat, alpha = 0.05, q_crit=NULL) {
  # Input validation
  k <- length(means)
  if (!is.matrix(covmat) || any(dim(covmat) != k)) {
    stop("covmat must be a square matrix with dimensions equal to length of means")
  }

  # Pairwise differences
  diff_mat <- outer(means, means, '-')

  # Standard errors for each pair: sqrt(var_i + var_j - 2*cov_ij)
  se_mat <- matrix(NA, k, k)
  for (i in 1:k) {
    for (j in 1:k) {
      se_mat[i, j] <- sqrt(covmat[i, i] + covmat[j, j] - 2 * covmat[i, j])
    }
  }

  # Critical value: use q = qtukey(1-alpha, k, df) as approximation
  # Degrees of freedom is not always clear in multivariate case; use large-sample approx
  if (is.null(q_crit)) {
    q_crit <- qtukey(1 - alpha, nmeans = k, df = Inf) # Conservative for large samples
  }

  # Confidence intervals
  lower <- diff_mat - q_crit * se_mat
  upper <- diff_mat + q_crit * se_mat

  # Test statistics and approximate p-values
  test_stat <- abs(diff_mat) / se_mat
  p_values <- 1 - ptukey(test_stat, nmeans = k, df = Inf)
  diag(p_values) <- 1
  p_values <- setZero((p_values+t(p_values))/2)

  return(list(
    differences = diff_mat,
    lower = lower,
    upper = upper,
    test_stat = test_stat,
    p_values = p_values,
    q_crit = q_crit
  ))
}
#
# The Multivariate Tukey-Kramer Multiple Comparison Procedure Among Four Correlated Mean Vectors
# Takashi Seo  & Takahiro Nishiyama
#
# On the conservative multivariate Tukey-Kramer type procedures for multiple comparisons among mean vectors
# Takashi Seo & Takahiro Nishiyama
