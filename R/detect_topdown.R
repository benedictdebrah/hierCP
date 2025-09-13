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
    cost_fun <- if (cost=="mean") segment_cost_mean else segment_cost_gaussian
    taus <- vector("list", ncol(Y))
    pc_map <- get_parent_child_map(hierarchy)

    # level 0 (aggregate level). If multiple series at level 0, process all.
    # Level-aware beta penalty
    beta_level <- function(level, n) {
      # Example: penalize deeper levels less
      log(n) / (1 + level)
    }
    j0 <- which(levels == 0)
    for (j in j0) {
      taus[[j]] <- detect_cp_window(Y[,j], w=w, beta=beta_level(levels[j], nrow(Y)), cost_fun=cost_fun)
    }

    for (ell in seq_len(L)) {
      jv <- which(levels == ell)
      for (j in jv) {
        parent_indices <- pc_map$parents[[j]]
        parent_tau <- if (length(parent_indices) > 0) unlist(taus[parent_indices]) else integer(0)
        tau_j <- detect_cp_window(
          Y[,j], w=w, beta=beta_level(levels[j], nrow(Y)), cost_fun=cost_fun,
          extra_penalty = if (eta > 0 && length(parent_tau) > 0) function(t) eta * misalignment_R(c(t), parent_tau) else NULL
        )

        if (soft_inherit_window > 0 && length(parent_tau)) {
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
