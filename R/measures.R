#' Compute regularized eigenvalue centrality from aggression matrix matrix.
#'
#' @param aggression_matrix An aggression matrix
#' @param epsilon Regularization term for computing eigenvalue centrality
#'
#' @return A vector of eigenvalue centrality for each individual in the aggression matrix
#' @export
#'
#' @examples
dom_ec <- function(aggression_matrix, epsilon = 0.694) {
  if (missing(aggression_matrix)) {
    stop("Please provide an aggression matrix as input.")
  } else {
    check_aggression_matrix(aggression_matrix)
  }
  t_mat <- transition_matrix(aggression_matrix, eps = epsilon)
  # Compute the eigenvalues and eigenvectors of the transpose
  # of the transition matrix (corresponds to the left hand
  # eigenvectors).
  evv <- eigen(t(t_mat), symmetric = FALSE)
  # Find the eigenvalue with the higest norm
  index_max <- which.max(abs(evv$values))
  # Return the normalized eigenvector
  return(abs(evv$vectors[index_max, ]) / sum(abs(evv$vectors[index_max, ])))
}
