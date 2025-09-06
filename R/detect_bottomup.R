#' Bottom-up hierarchical changepoint detection
#' @param Y T x p matrix
#' @param hierarchy hierarchy object
#' @param eta misalignment penalty (placeholder)
#' @param w window
#' @param soft_aggregate_window window around child cps to allow at parent
#' @param cost "mean" or "gaussian"
#' @return list of cps per series
#' @export
detect_cp_bottomup <- function(Y, hierarchy, eta = 0, w = 20,
                               soft_aggregate_window = 5, cost = "mean") {
  levels <- hierarchy$levels
  L <- max(levels)
  idx_by_level <- split(seq_len(ncol(Y)), levels)
  cost_fun <- if (cost=="mean") segment_cost_mean else segment_cost_gaussian

  taus <- vector("list", ncol(Y))

  # Start at bottom level
  jB <- idx_by_level[[as.character(L)]]
  for (j in jB) {
    taus[[j]] <- detect_cp_window(Y[,j], w=w, beta=log(nrow(Y)), cost_fun=cost_fun)
  }

  # Aggregate up
  for (ell in rev(seq_len(L)) - 1) { # from L-1 down to 0
    jv <- idx_by_level[[as.character(ell)]]
    for (j in jv) {
      # gather child cps (naively: next level indices near j)
      child_level <- ell + 1
      children <- idx_by_level[[as.character(child_level)]]

      # candidate set is union of child cps, maybe widened by window
      child_union <- sort(unique(unlist(taus[children])))
      if (length(child_union) && soft_aggregate_window > 0) {
        cand <- sort(unique(unlist(lapply(child_union, function(t)
          max(1, t - soft_aggregate_window):min(nrow(Y)-1, t + soft_aggregate_window)))))
      } else {
        cand <- integer(0)
      }

      tau_j <- detect_cp_window(Y[,j], w=w, beta=log(nrow(Y)), cost_fun=cost_fun)
      if (length(cand)) {
        # filter to be near child candidates
        keep <- logical(length(tau_j))
        for (k in seq_along(tau_j)) keep[k] <- tau_j[k] %in% cand
        if (any(keep)) tau_j <- tau_j[keep]
      }
      taus[[j]] <- unique(tau_j)
    }
  }
  taus
}
