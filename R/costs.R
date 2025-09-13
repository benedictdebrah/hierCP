
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

#' Fast segment cost: change in mean (SSE) using cumulative sums
#' @keywords internal
segment_cost_mean_fast <- function(cum_x, cum_x2, a, b) {
  n <- b - a + 1
  sum_x <- cum_x[b] - if (a > 1) cum_x[a-1] else 0
  sum_x2 <- cum_x2[b] - if (a > 1) cum_x2[a-1] else 0
  m <- sum_x / n
  sum_x2 - 2 * m * sum_x + n * m^2
}

#' Fast segment cost: Gaussian log-variance using cumulative sums
#' @keywords internal
segment_cost_gaussian_fast <- function(cum_x, cum_x2, a, b) {
  n <- b - a + 1
  sum_x <- cum_x[b] - if (a > 1) cum_x[a-1] else 0
  sum_x2 <- cum_x2[b] - if (a > 1) cum_x2[a-1] else 0
  m <- sum_x / n
  v <- (sum_x2 - 2 * m * sum_x + n * m^2) / n
  n * log(v + 1e-12)
}