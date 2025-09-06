#' Fit GM(1,1) model
#' @param x numeric vector (length >= 4)
#' @return list with parameters
#' @export
gm11_fit <- function(x) {
  stopifnot(length(x) >= 4)
  X0 <- x
  X1 <- cumsum(X0)
  Z1 <- (X1[-1] + X1[-length(X1)]) / 2
  B  <- cbind(-Z1, 1)
  Y  <- X0[-1]
  theta <- solve(t(B) %*% B, t(B) %*% Y)
  a <- theta[1]; b <- theta[2]
  list(a=a, b=b, x1_1 = X0[1])
}

#' Forecast GM(1,1)
#' @param fit model from gm11_fit
#' @param h horizon
#' @param n_obs number of observed points
#' @export
gm11_forecast <- function(fit, h, n_obs) {
  a <- fit$a; b <- fit$b; x1_1 <- fit$x1_1
  k <- (n_obs):(n_obs+h-1)
  x1k1 <- (x1_1 - b/a) * exp(-a * (k-1)) - b/a
  x0hat <- diff(c((x1_1 - b/a), x1k1))
  as.numeric(x0hat)
}
