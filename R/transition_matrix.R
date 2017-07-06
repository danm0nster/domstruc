#' Compute the transition matrix.
#'
#' The transition matrix is computed from the aggression matrix. The element T[i,j] of
#' the transition matrix is the probability that individual i attacks individual j.
#'
#' @param A aggression matrix
#' @param eps the epsilon parameter
#' @return The transition matrix

ComputeTransitionMatrix <- function(A, eps = 1e-6) {
  # Check the input parameters
  if (missing(A)) {
    stop("Please provide an aggression matrix as input.")
  } else {
    if (!is.matrix(A))
      stop("A must be a matrix.")
  }
  if (eps > 1) {
    stop("eps must be less than or equal to 1.")
  } else if (eps < 0) {
    stop("eps must be positive.")
  } else if (eps < .Machine$double.eps) {
    stop("You specified eps less that machine precision.")
  }
  as.matrix(0)
}
