#' Mean Absolute Scaled Error
#' @export
MASE <- function(y, yhat, s = 1) {
  n <- length(y)
  scale <- mean(abs(y[(s+1):n] - y[1:(n-s)]))
  mean(abs(y - yhat)) / (scale + 1e-12)
}

#' Root Mean Squared Scaled Error
#' @export
RMSSE <- function(y, yhat, s = 1) {
  n <- length(y)
  scale <- mean((y[(s+1):n] - y[1:(n-s)])^2)
  sqrt(mean((y - yhat)^2) / (scale + 1e-12))
}

#' Absolute Mean Scaled Error
#' @export
AMSE <- function(y, yhat, s = 1) {
  n <- length(y)
  scale <- mean(abs(y[(s+1):n] - y[1:(n-s)]))
  abs(mean(y - yhat)) / (scale + 1e-12)
}
