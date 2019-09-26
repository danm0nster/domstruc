#' Compute regularized eigenvalue centrality from aggression matrix
#'
#' @param aggression_matrix An aggression matrix.
#' @param epsilon Regularization term for computing eigenvalue centrality.
#'
#' @return A vector of eigenvalue centrality for each individual in the
#' aggression matrix.
#' @export
#'
#' @examples
dom_ec <- function(aggression_matrix, epsilon = 0.694) {
  if (missing(aggression_matrix)) {
    stop("Please provide an aggression matrix as input.")
  } else {
    check_aggression_matrix(aggression_matrix)
  }
  check_epsilon(epsilon)
  t_mat <- dom_transition_matrix(aggression_matrix, epsilon = epsilon)
  # Compute the eigenvalues and eigenvectors of the transpose
  # of the transition matrix (corresponds to the left hand
  # eigenvectors).
  evv <- eigen(t(t_mat), symmetric = FALSE)
  # Find the eigenvalue with the higest norm
  index_max <- which.max(abs(evv$values))
  # Return the normalized eigenvector
  return(abs(evv$vectors[, index_max]) / sum(abs(evv$vectors[, index_max])))
}

#' Compute focus from an aggression matrix
#'
#' @param aggression_matrix An aggression matrix.
#' @param epsilon Regularization term for computing eigenvalue centrality.
#'
#' @return the computed focus value.
#' @export
#'
#' @examples
dom_focus <- function(aggression_matrix, epsilon = 0.694) {
  if (missing(aggression_matrix)) {
    stop("Please provide an aggression matrix as input.")
  } else {
    check_aggression_matrix(aggression_matrix)
  }
  check_epsilon(epsilon)
  # Compute rank focused aggression
  focused_agg <- dom_rank_focused_aggression(aggression_matrix, epsilon = epsilon)
  # Compute the necessary terms for focus as new columns in the data frame
  focused_agg$focus_terms <- 0
  # Select the rows where agg_norm is non-zero
  idx <- focused_agg$agg_norm > 0
  focused_agg$focus_terms[idx] <- focused_agg$delta[idx] *
    focused_agg$agg[idx] / focused_agg$agg_norm[idx]
  denom <- sum(focused_agg$r_delta)
  mu <- sum(focused_agg$focus_terms) / denom
  focused_agg$focus_var <- 0
  focused_agg$focus_var[idx] <- focused_agg$agg[idx] *
    (focused_agg$delta[idx] - mu)**2 / focused_agg$agg_norm[idx]
  n <- dim(aggression_matrix)[1]
  return(1 - (sum(focused_agg$focus_var) / denom) / (n * (2 * n - 1) / 6.0))
}

#' Compute position from an aggression matrix
#'
#' @param aggression_matrix An aggression matrix.
#' @param epsilon Regularization term for computing eigenvalue centrality.
#'
#' @return The position, i.e. the relative rank difference where the aggression is most focused.
#' @export
#'
#' @examples
dom_position <- function(aggression_matrix, epsilon = 0.694) {
  if (missing(aggression_matrix)) {
    stop("Please provide an aggression matrix as input.")
  } else {
    check_aggression_matrix(aggression_matrix)
  }
  check_epsilon(epsilon)
  # Compute rank focused aggression
  focused_agg <- dom_rank_focused_aggression(aggression_matrix, epsilon = epsilon)
  tiny <- 1e-12
  # Calculate the terms needed to compute position
  focused_agg$fraction <- focused_agg$delta * focused_agg$agg /
    (focused_agg$agg_norm + tiny)
  focused_agg$norm <- focused_agg$agg / (focused_agg$agg_norm + tiny)
  n <- dim(aggression_matrix)[1]
  return( mean(focused_agg$fraction) / mean(focused_agg$norm) / n)
}
