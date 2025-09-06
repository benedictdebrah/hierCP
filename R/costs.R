#' Segment cost: change in mean (SSE)
#' @keywords internal
segment_cost_mean <- function(x, a, b) {
  seg <- x[a:b]
  m <- mean(seg)
  sum((seg - m)^2)
}

#' Segment cost: Gaussian log-variance
#' @keywords internal
segment_cost_gaussian <- function(x, a, b) {
  seg <- x[a:b]; n <- length(seg)
  n * log(var(seg) + 1e-12)
}

#' Misalignment penalty R(tau_i, tau_j)
#' @keywords internal
misalignment_R <- function(tau_i, tau_j) {
  if (length(tau_i) == 0 || length(tau_j) == 0) return(0)
  sum(vapply(tau_j, function(tj) min(abs(tau_i - tj)), numeric(1)))
}
