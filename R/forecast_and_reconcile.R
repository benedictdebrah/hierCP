# forecast_and_reconcile.R
# Minimal forecasting: last-segment mean repeated h steps; reconciliation by summing matrix

last_segment <- function(x, cps) {
  if (!length(cps)) return(x)
  x[(max(cps)+1):length(x)]
}


# Reconcile: build all-series from bottom forecasts via S
reconcile_sum <- function(yhatB, S) {
  # yhatB: h x nb; return h x nt, columns in order: bottom then aggregates
  nb <- ncol(S); nt <- nrow(S)
  Yall_t <- S %*% t(yhatB)      # nt x h
  t(Yall_t)                     # h x nt
}

# One-call pipeline: detect CP independently, forecast bottoms, sum to all series
fit_forecast <- function(Y, hierarchy, h = 6, w = 20, cost = c("mean","gaussian")) {
  cost <- match.arg(cost)
  S  <- hierarchy$S; nb <- hierarchy$nb
  lev <- hierarchy$levels

  taus <- detect_cp_independent(Y, lev, w = w, cost = cost)

  # bottom forecasts
  yhatB <- matrix(NA_real_, nrow = h, ncol = nb)
  for (i in seq_len(nb)) {
    seg <- last_segment(Y[, i], taus[[i]])
    yhatB[, i] <- base_forecast_last_segment(seg, h = h)
  }

  # coherent all-series
  ytilde <- reconcile_sum(yhatB, S)
  colnames(ytilde) <- hierarchy$names
  list(taus = taus, yhatB = yhatB, ytilde = ytilde)
}
