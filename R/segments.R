#' Extract last segment given changepoints
#' @param x numeric vector
#' @param tau integer vector of cps
#' @export
last_segment <- function(x, tau) {
  if (length(tau) == 0) return(x)
  x[(tau[length(tau)] + 1):length(x)]
}
