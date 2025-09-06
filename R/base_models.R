#' Choose base model based on segment length
#' @param x numeric vector
#' @param h horizon
#' @param n_star threshold for GM(1,1) vs ARIMA
#' @export
base_forecast_last_segment <- function(x, h, n_star = 8) {
  seg <- x
  if (length(seg) <= n_star && length(seg) >= 4) {
    fit <- gm11_fit(seg)
    gm11_forecast(fit, h=h, n_obs=length(seg))
  } else {
    arima_lastseg_forecast(seg, h=h)
  }
}
