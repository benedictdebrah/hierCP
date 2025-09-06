#' Top-down hierarchical changepoint detection
#' @param Y T x p matrix
#' @param hierarchy hierarchy object
#' @param eta misalignment penalty weight (currently placeholder)
#' @param w window
#' @param soft_inherit_window integer; if >0, allow children to search near parent cps
#' @param cost "mean" or "gaussian"
#' @return list of changepoints per series
#' @export
detect_cp_topdown <- function(Y, hierarchy, eta = 0, w = 20,
                              soft_inherit_window = 5, cost = "mean") {
  levels <- hierarchy$levels
  L <- max(levels)
  idx_by_level <- split(seq_len(ncol(Y)), levels)
  cost_fun <- if (cost=="mean") segment_cost_mean else segment_cost_gaussian

  taus <- vector("list", ncol(Y))

  # level 0 (aggregate level). If multiple series at level 0, process all.
  j0 <- idx_by_level[["0"]]
  for (j in j0) {
    taus[[j]] <- detect_cp_window(Y[,j], w=w, beta=log(nrow(Y)), cost_fun=cost_fun)
  }

  # For demonstration, we assume a simple parent-child mapping by index proximity.
  # In a real setting, pass a mapping list in `hierarchy`.
  parent_of <- function(j) {
    # naive: previous level first index
    pl <- levels[j] - 1
    if (pl < 0) return(NA_integer_)
    cand <- idx_by_level[[as.character(pl)]]
    cand[1]
  }

  for (ell in seq_len(L)) {
    jv <- idx_by_level[[as.character(ell)]]
    for (j in jv) {
      pidx <- parent_of(j)
      parent_tau <- if (!is.na(pidx)) taus[[pidx]] else integer(0)
      tau_j <- detect_cp_window(Y[,j], w=w, beta=log(nrow(Y)), cost_fun=cost_fun)

      if (soft_inherit_window > 0 && length(parent_tau)) {
        # keep only cps near parent cps (soft inheritance)
        keep <- logical(length(tau_j))
        for (k in seq_along(tau_j)) {
          keep[k] <- any(abs(parent_tau - tau_j[k]) <= soft_inherit_window)
        }
        if (any(keep)) tau_j <- tau_j[keep]
      }
      taus[[j]] <- unique(tau_j)
    }
  }
  taus
}
