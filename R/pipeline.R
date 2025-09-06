#' High-level pipeline: detect CPs, base-forecast, reconcile
#' @param Y T x p matrix (columns are series, bottom series first in columns 1:nb, then upper series)
#' @param hierarchy hierarchy object from new_hierarchy()
#' @param h forecast horizon
#' @param strategy "independent" | "topdown" | "bottomup"
#' @export
fit_forecast <- function(Y, hierarchy, h=6, strategy="topdown",
                         eta=0, w=20, soft_inherit_window=5,
                         cost="mean", n_star=8,
                         weights=c(bottom=0.6, mid=0.2, top=0.2),
                         tune_lambda=TRUE,
                         reconciliation="sum") {

  # detect cps
  taus <- switch(strategy,
    independent = detect_cp_independent(Y, hierarchy$levels, w=w, cost=cost),
    topdown     = detect_cp_topdown(Y, hierarchy, eta=eta, w=w,
                                    soft_inherit_window=soft_inherit_window, cost=cost),
    bottomup    = detect_cp_bottomup(Y, hierarchy, eta=eta, w=w,
                                     soft_aggregate_window=soft_inherit_window, cost=cost)
  )

  # bottom forecasts using last segments
  nb <- hierarchy$nb; S <- hierarchy$S
  yhatB <- matrix(NA_real_, nrow=h, ncol=nb)
  for (i in seq_len(nb)) {
    seg <- last_segment(Y[,i], taus[[i]])
    yhatB[,i] <- base_forecast_last_segment(seg, h=h, n_star=n_star)
  }

  # structured regularization (placeholder tuning)
  sr <- structured_fit(Y=Y, hierarchy=hierarchy, h=h,
                       weights=weights, tune=tune_lambda)

  # reconciliation
  if (reconciliation == "sum") {
    ytilde <- reconcile(yhatB, S)
  } else if (reconciliation == "ols") {
    ytilde <- reconcile_ols(yhatB, S)
  } else if (reconciliation == "none") {
    ytilde <- yhatB
  } else {
    stop(sprintf("Unknown reconciliation method: %s", reconciliation))
  }
  colnames(ytilde) <- hierarchy$names

  list(taus=taus, yhatB=yhatB, ytilde=ytilde, lambda=sr$lambda)
}
