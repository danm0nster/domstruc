#' Compute the transition matrix.
#'
#' The transition matrix is computed from the aggression matrix. The element T[i,j] of
#' the transition matrix is the probability that individual i attacks individual j.
#'
#' @param A aggression matrix
#' @param eps the epsilon parameter
#' @return The transition matrix

ComputeTransitionMatrix <- function(A, eps = 1e-12) {
  # Check the input parameters
  if (missing(A)) {
    stop("Please provide an aggression matrix as input.")
  } else {
    if (!is.matrix(A))
      stop("A must be a matrix.")
      # TODO(danm0nster): possibly allow for arrays and data frames
    else {
      rows <- dim(A)[1]
      cols <- dim(A)[2]
      if (rows != cols) {
        stop("Aggression matrix must be square.")
      } else if (dim(A)[1] < 2) {
        stop("Aggression matrix dimension must be greater than 1.")
      }
    }
  }
  if (eps > 1) {
    stop("eps must be less than or equal to 1.")
  } else if (eps < 0) {
    stop("eps must be positive.")
  } else if (eps < .Machine$double.eps) {
    stop("You specified eps less that machine precision. Please increase value.")
  }
  # Checks are done, and input should be fine, so we can move on to the actual computations.
  ones <- matrix(1, nrow = rows, ncol = rows)
  ident <- diag(rows)
  numerator <- A + eps * (ones - ident)
  row.sum <- rowSums(A)
  # Repeat this as a column, so each element contains the row sum.
  denominator <- matrix(rep(row.sum,each = cols), ncol = cols, byrow = TRUE)
  denominator <- denominator + rows * eps
  numerator / denominator
}
