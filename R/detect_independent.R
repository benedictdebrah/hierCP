#' Detect changepoints independently per series
#' @param Y T x p matrix (columns = series)
#' @param levels integer vector of length p giving levels per series (0..L)
#' @param betas optional penalties per series
#' @param w window size
#' @param cost "mean" or "gaussian"
#' @return list of integer vectors of changepoints per series
#' @export
detect_cp_independent <- function(Y, levels, betas = NULL, w = 20,
                                  cost = c("mean","gaussian")) {
  cost <- match.arg(cost)
  cost_fun <- if (cost=="mean") segment_cost_mean else segment_cost_gaussian
  Tn <- nrow(Y); p <- ncol(Y)
  if (is.null(betas)) betas <- rep(log(Tn), p)
  taus <- vector("list", p)
  for (j in seq_len(p)) {
    taus[[j]] <- detect_cp_window(Y[,j], w=w, beta=betas[j], cost_fun=cost_fun)
  }
  taus
}
