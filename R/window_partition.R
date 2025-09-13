detect_cp_window <- function(x, w = 20, beta = NULL,
                             cost_fun = segment_cost_mean, max_cp = Inf, extra_penalty = NULL) {
  n <- length(x); if (is.null(beta)) beta <- log(n)
  F <- rep(Inf, n+1); F[1] <- -beta  # DP array: cost up to t
  cp_back <- integer(n+1)
  # Precompute cumulative sums for fast segment cost
  cum_x <- cumsum(x)
  cum_x2 <- cumsum(x^2)
  # Memoization table for segment costs
  cost_cache <- new.env(hash=TRUE, parent=emptyenv())
  # Choose fast cost function if possible
  use_fast <- identical(cost_fun, segment_cost_mean) || identical(cost_fun, segment_cost_gaussian)
  fast_fun <- if (identical(cost_fun, segment_cost_mean)) segment_cost_mean_fast else if (identical(cost_fun, segment_cost_gaussian)) segment_cost_gaussian_fast else NULL
  Rset <- 1
  for (t in seq_len(n)) {
    best_val <- Inf; best_j <- 0
    for (j in Rset) {
      if (j > t) next
      key <- paste(j, t, sep=":")
      if (exists(key, envir=cost_cache, inherits=FALSE)) {
        seg_cost <- get(key, envir=cost_cache)
      } else if (use_fast && !is.null(fast_fun)) {
        seg_cost <- fast_fun(cum_x, cum_x2, j, t)
        assign(key, seg_cost, envir=cost_cache)
      } else {
        seg_cost <- cost_fun(x, j, t)
        assign(key, seg_cost, envir=cost_cache)
      }
      penalty <- if (!is.null(extra_penalty)) extra_penalty(j-1) else 0
      cval <- F[j-1] + seg_cost + beta + penalty
      if (!is.null(cval) && length(cval) > 0 && !is.na(cval)) {
        if (cval < best_val) { best_val <- cval; best_j <- j }
      }
    }
    F[t] <- best_val
    cp_back[t] <- best_j
    # PELT pruning: keep only candidates that satisfy the prune condition
  valid_Rset <- Rset[Rset - 1 >= 1 & Rset <= t]

    keep <- vapply(valid_Rset, function(j) {
      key <- paste(j, t, sep=":")
      if (exists(key, envir=cost_cache, inherits=FALSE)) {
        seg_cost <- get(key, envir=cost_cache)
      } else if (use_fast && !is.null(fast_fun)) {
        seg_cost <- fast_fun(cum_x, cum_x2, j, t)
        assign(key, seg_cost, envir=cost_cache)
      } else {
        seg_cost <- cost_fun(x, j, t)
        assign(key, seg_cost, envir=cost_cache)
      }
      F[j-1] + seg_cost + beta < F[t]
    }, logical(1))
    Rset <- c(valid_Rset[keep], t+1)
    # Optionally, limit Rset size for memory (window capping)
    if (length(Rset) > w) {
      Rset <- tail(Rset, w)
    }
  }

  # backtrack
  tau <- integer(0); t <- n
  while (t > 0) {
    j <- cp_back[t]
    if (j <= 1) break
    tau <- c(j-1, tau)
    t <- j-1
  }
  if (length(tau) > max_cp) tau <- tau[seq_len(max_cp)]
  unique(tau)
}