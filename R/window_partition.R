#' Windowed optimal partition (basic PELT-style) changepoint detection
#' @param x numeric vector
#' @param w window size for candidate starts
#' @param beta penalty per changepoint (default log(n))
#' @param cost_fun function(x,a,b) returning segment cost
#' @param max_cp maximum number of changepoints to return
#' @return integer vector of changepoint locations (end indices of segments)
#' @export
detect_cp_window <- function(x, w = 20, beta = NULL,
                             cost_fun = segment_cost_mean, max_cp = Inf) {
  n <- length(x); if (is.null(beta)) beta <- log(n)
  F <- rep(Inf, n+1); F[1] <- -beta  # DP array: cost up to t
  cp_back <- integer(n+1)
  candidates <- 1:n

  for (t in seq_len(n)) {
    j_start <- max(1, t - w + 1)
    # simple pruning buffer
    best_val <- Inf; best_j <- 0
    for (j in j_start:t) {
      cval <- F[j-1] + cost_fun(x, j, t) + beta
      if (!is.null(cval) && length(cval) > 0 && !is.na(cval)) {
        if (cval < best_val) { best_val <- cval; best_j <- j }
      } else {
        # Optionally, handle the case where cval is invalid
        # For now, skip this candidate
        next
      }
    }
    F[t] <- best_val
    cp_back[t] <- best_j
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
