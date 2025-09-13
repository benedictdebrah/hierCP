#' Create a hierarchy object
#' @param S Summing matrix (n_total x n_bottom), bottom series must come first in row order.
#' @param levels Integer vector of length n_total giving hierarchy level per series (0 = top/aggregate).
#' @param series_names Optional character vector of series names (length n_total).
#' @return An object of class "hierCP_hierarchy".
#' @export
new_hierarchy <- function(S, levels, series_names = NULL) {
  stopifnot(is.matrix(S))
  nb <- ncol(S)
  nt <- nrow(S)
  levels <- as.integer(levels)
  if (length(levels) != nt) stop("levels must have length n_total (nrow(S)).")
  if (is.null(series_names)) series_names <- paste0("y", seq_len(nt))
  structure(
    list(S=S, nb=nb, nt=nt, levels=levels, names=series_names),
    class="hierCP_hierarchy"
  )
}

#' Construct H (upper rows of S beyond bottom series)
#' @param S Summing matrix.
#' @return Matrix H.
#' @export
H_from_S <- function(S) {
  nb <- ncol(S); nt <- nrow(S)
  if (nb >= nt) stop("S must have more rows (total series) than columns (bottom series).")
  S[(nb+1):nt, , drop=FALSE]
}

#' Extract parent/child relationships from hierarchy object
#' @param hierarchy hierarchy object (from new_hierarchy)
#' @return list with $parents and $children (indices per series)
#' @export
get_parent_child_map <- function(hierarchy) {
  S <- hierarchy$S
  levels <- hierarchy$levels
  nt <- nrow(S)
  nb <- ncol(S)
  # Compute bottom coverage for each node
  bottoms <- lapply(seq_len(nt), function(i) which(S[i, ] != 0))
  parents <- vector("list", nt)
  children <- vector("list", nt)
  for (i in seq_len(nt)) {
    # Parents: nodes at previous level whose bottom coverage is a superset of i's
    parents[[i]] <- which(
      levels == (levels[i] - 1) &
      vapply(seq_len(nt), function(j) {
        all(bottoms[[i]] %in% bottoms[[j]]) && length(bottoms[[i]]) > 0 && i != j
      }, logical(1))
    )
    # Children: nodes at next level whose bottom coverage is a subset of i's
    children[[i]] <- which(
      levels == (levels[i] + 1) &
      vapply(seq_len(nt), function(j) {
        all(bottoms[[j]] %in% bottoms[[i]]) && length(bottoms[[j]]) > 0 && i != j
      }, logical(1))
    )
  }
  list(parents = parents, children = children)
}
