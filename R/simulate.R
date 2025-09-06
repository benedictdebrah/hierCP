#' Simulate a simple 2-level hierarchy with known cps
#' @param Tn length of series
#' @param nb number of bottom series
#' @export
sim_hier_data <- function(Tn=120, nb=3, cps_bottom=list(c(40,90), c(60), c(70)), noise=1.0) {
  # bottom series
  YB <- matrix(NA_real_, nrow=Tn, ncol=nb)
  set.seed(1L)
  for (i in seq_len(nb)) {
    mu <- rep(0, Tn)
    last <- 0; cps <- sort(unique(pmax(2, pmin(Tn-2, cps_bottom[[i]]))))
    pts <- c(0, cps, Tn)
    lvl <- 0
    for (k in seq_len(length(pts)-1)) {
      lvl <- lvl + runif(1, -1, 1)
      mu[(pts[k]+1):pts[k+1]] <- lvl
    }
    YB[,i] <- mu + rnorm(Tn, sd=noise)
  }
  # S: total series = nb (bottom) + 1 top
  S <- rbind(diag(nb), matrix(1, nrow=1, ncol=nb))
  levels <- c(rep(1L, nb), 0L) # bottoms are level 1, top is level 0
  # build top as sum
  Ytop <- rowSums(YB)
  Y <- cbind(YB, Ytop)
  list(Y=Y, S=S, levels=levels)
}
