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
  cost_fun <- if (cost=="mean") segment_cost_mean else segment_cost_gaussian
  taus <- vector("list", ncol(Y))
  pc_map <- get_parent_child_map(hierarchy)

  # Start at bottom level
  # Level-aware beta penalty
  beta_level <- function(level, n) {
    log(n) / (1 + level)
  }
  jB <- which(levels == L)
  for (j in jB) {
    taus[[j]] <- detect_cp_window(Y[,j], w=w, beta=beta_level(levels[j], nrow(Y)), cost_fun=cost_fun)
  }

  # Aggregate up
  for (ell in rev(seq_len(L)) - 1) { # from L-1 down to 0
    jv <- which(levels == ell)
    for (j in jv) {
      children_indices <- pc_map$children[[j]]
      child_tau <- if (length(children_indices) > 0) unlist(taus[children_indices]) else integer(0)

      # candidate set is union of child cps, maybe widened by window
      child_union <- sort(unique(child_tau))
      if (length(child_union) && soft_aggregate_window > 0) {
        cand <- sort(unique(unlist(lapply(child_union, function(t)
          max(1, t - soft_aggregate_window):min(nrow(Y)-1, t + soft_aggregate_window)))))
      } else {
        cand <- integer(0)
      }

      tau_j <- detect_cp_window(
        Y[,j], w=w, beta=beta_level(levels[j], nrow(Y)), cost_fun=cost_fun,
        extra_penalty = if (eta > 0 && length(child_tau) > 0) function(t) eta * misalignment_R(c(t), child_tau) else NULL
      )
      if (length(cand)) {
        keep <- logical(length(tau_j))
        for (k in seq_along(tau_j)) keep[k] <- tau_j[k] %in% cand
        if (any(keep)) tau_j <- tau_j[keep]
      }
      taus[[j]] <- unique(tau_j)
    }
  }
  taus
}
