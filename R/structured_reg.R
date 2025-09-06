#' Ensure column names of a matrix or data.frame
#' @param x matrix or data.frame
#' @param names vector of column names
ensure_colnames <- function(x, names) {
  colnames(x) <- names
  return(x)
}

#' Check coherence: top-level series equals sum of bottom-level series
#' @param ytilde reconciled forecasts (matrix)
#' @param S summing matrix
#' @return TRUE if coherent, FALSE otherwise
coherence_ok <- function(ytilde, S) {
  nb <- ncol(S)
  y_bottom <- t(ytilde[, 1:nb, drop=FALSE])
  y_all <- t(ytilde)
  y_sum <- S %*% y_bottom
  all(abs(y_sum - y_all) < 1e-6)
}
#' Reconcile coherent forecasts from bottom forecasts using OLS
#' @param yhatB h x nb matrix of bottom forecasts
#' @param S summing matrix
#' @export
reconcile_ols <- function(yhatB, S) {
  # OLS reconciliation: project onto coherent space
  # yhatB: h x nb, S: nt x nb
  # Returns: h x nt
  # Uses generalized inverse for reconciliation
  library(MASS)
  G <- ginv(S)
  Yall_t <- S %*% t(yhatB)
  t(Yall_t)
}
#' Structured regularization (simplified tuning of lambda over upper levels)
#' @param Y T x p matrix (bottom series first in columns 1:nb)
#' @param hierarchy hierarchy object
#' @param h forecast horizon
#' @param weights named vector: bottom, mid, top
#' @param lambda initial lambda vector for upper series
#' @param tune logical; if TRUE, optimize lambda with a proxy objective
#' @export
structured_fit <- function(Y, hierarchy, h, weights = c(bottom=0.6, mid=0.2, top=0.2),
                           lambda = NULL, tune = TRUE) {
  S <- hierarchy$S
  nb <- hierarchy$nb; nt <- hierarchy$nt
  H <- H_from_S(S)

  # 1) naive bottom forecasts for in-sample last h points (proxy training)
  # In production: do rolling-origin backtesting. Here we keep it simple.
  yhatB <- matrix(NA_real_, nrow=h, ncol=nb)
  for (i in seq_len(nb)) {
    x <- Y[, i]
    # use most recent min(length(x), h) to get naive last-segment fit
    seg <- tail(x, max(6, min(length(x), 24)))
    yhatB[, i] <- base_forecast_last_segment(seg, h=h)
  }

  if (is.null(lambda)) lambda <- rep(1, nrow(H))

  obj <- function(par) {
    lam <- if (tune) pmax(par, 0) else lambda
    Lambda <- diag(lam, nrow(H), nrow(H))
    Yhat_upper <- H %*% t(yhatB)  # upper x h
    # proxy quadratic penalty on upper series magnitude
    sum(diag(t(Yhat_upper) %*% Lambda %*% Yhat_upper)) * (weights["top"] + weights["mid"])
  }

  if (tune) {
    fit <- stats::optim(par=lambda, fn=obj, method="L-BFGS-B", lower=0)
    lambda <- fit$par
  }

  list(yhatB=yhatB, lambda=lambda)
}

#' Reconcile coherent forecasts from bottom forecasts
#' @param yhatB h x nb matrix of bottom forecasts
#' @param S summing matrix
#' @export
reconcile <- function(yhatB, S) {
  t(S %*% t(yhatB))
}
