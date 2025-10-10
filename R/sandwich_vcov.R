#' sandwich_vcov
#'
#' @param model
#' @param type
#'
#' @return
#' @export
#'
#' @examples
#' # For OLS
#' lm_fit <- lm(mpg ~ wt + hp, data = mtcars)
#' vcov_lm <- sandwich_vcov(lm_fit)
#'
#' # For GLM
#' glm_fit <- glm(am ~ wt, data = mtcars, family = binomial)
#' vcov_glm <- sandwich_vcov(glm_fit)
sandwich_vcov <- function(model, ...) {
  UseMethod("sandwich_vcov")
}

#' @export
sandwich_vcov.lm <- function(model, type = "HC0", ...) {
  if (!inherits(model, "lm")) stop("Works for lm/glm only")

  X <- model.matrix(model)
  n <- nrow(X)
  p <- ncol(X)

  # Get residuals and adjust for GLM
  if (inherits(model, "glm")) {
    resid <- residuals(model, "working") * sqrt(weights(model, "working"))
  } else {
    resid <- residuals(model)
  }

  # Compute meat: X'ΩX (Ω = diag(resid^2))
  meat <- crossprod(X * resid)

  # Compute bread: (X'X)^{-1} for lm, (X'WX)^{-1} for glm
  bread <- vcov(model)
  if (inherits(model, "glm")) {
    bread <- bread * summary(model)$dispersion
  } else {
    bread <- bread * (n - p) / sum(resid^2)
  }

  # Sandwich covariance
  vcov_sand <- bread %*% meat %*% bread

  # HC1 correction (n/(n-p))
  if (type == "HC1") vcov_sand <- vcov_sand * n / (n - p)

  return(vcov_sand)
}

# lmer_fit <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
# vcov_lmer <- lmer_sandwich(lmer_fit)

#' @export
sandwich_vcov.merMod <- function(model, cluster=NULL, ...) {

  sandwich_vcov_cluster <- function(model, fixef_only=TRUE, cluster_id=cluster, ...) {
    if (!inherits(model, "merMod")) stop("Requires lme4 model")

    # Get fixed effects design matrix and residuals
    X <- model.matrix(model)
    resid <- residuals(model, type = "response")

    # Cluster indices (e.g., group IDs)
    clusters <- model@frame[[cluster_id]]
    unique_clusters <- unique(clusters)

    # Compute meat: Sum of cluster-specific score crossproducts
    meat <- matrix(0, ncol(X), ncol(X))
    for (cl in unique_clusters) {
      idx <- which(clusters == cl)
      X_cl <- X[idx, , drop = FALSE]
      resid_cl <- resid[idx]
      meat <- meat + t(X_cl) %*% outer(resid_cl, resid_cl) %*% X_cl
    }

    # Bread: Inverse of expected Hessian (fixed effects part)
    bread <- vcov(model)

    # Sandwich covariance
    vcov_sand <- bread %*% meat %*% bread
    return(vcov_sand)
  }

  if (!is.null(cluster)) {
    sandwich_vcov_cluster(model, fixef_only, cluster, ...)
  }

  if (!inherits(model, "merMod")) stop("Requires lme4 model")

  # Extract Hessian
  hessian <- model@optinfo$derivs$Hessian

  # Check positive definiteness
  if (any(eigen(hessian)$values <= 0)) {
    warning("Hessian not positive definite; results may be unreliable.")
  }

  # Compute covariance matrix
  vcov_full <- solve(hessian)

  # Name dimensions (fixed + random parameters)
  p_fix <- length(fixef(model))
  p_theta <- length(getME(model, "theta"))
  rownames(vcov_full) <- colnames(vcov_full) <-
    c(names(fixef(model)), paste0("theta_", seq_len(p_theta)))

  return(vcov_full)
}
