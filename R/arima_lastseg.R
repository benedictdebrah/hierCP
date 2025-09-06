#' ARIMA forecast on last segment
#' @param x numeric vector
#' @param h horizon
#' @export
arima_lastseg_forecast <- function(x, h, n_star = NULL) {
  fit <- forecast::auto.arima(x, stepwise=FALSE, approximation=TRUE)
  as.numeric(forecast::forecast(fit, h=h)$mean)
}
