#' Plot hierarchy structure (rows of S)
#' @export
plot_hierarchy <- function(hierarchy) {
  S <- hierarchy$S
  df <- data.frame(row=rep(seq_len(nrow(S)), times=ncol(S)),
                   col=rep(seq_len(ncol(S)), each=nrow(S)),
                   val=as.vector(S))
  ggplot2::ggplot(df, ggplot2::aes(x=col, y=row, fill=val)) +
    ggplot2::geom_tile() +
    ggplot2::theme_minimal() +
    ggplot2::labs(x="Bottom series", y="All series (rows of S)", title="Summing matrix S (structure)")
}

#' Plot a series with detected changepoints
#' @export
plot_series_cps <- function(x, tau, title="Series with changepoints") {
  df <- data.frame(t=seq_along(x), y=x)
  g <- ggplot2::ggplot(df, ggplot2::aes(x=t, y=y)) +
    ggplot2::geom_line() +
    ggplot2::theme_minimal() + ggplot2::labs(title=title, x="t", y="y")
  if (length(tau)) {
    for (cp in tau) g <- g + ggplot2::geom_vline(xintercept=cp, linetype="dashed")
  }
  g
}
