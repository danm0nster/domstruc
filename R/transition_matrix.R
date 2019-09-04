#' Compute the transition matrix.
#'
#' The transition matrix is computed from the aggression matrix. The element T[i,j] of
#' the transition matrix is the probability that individual i attacks individual j.
#'
#' @param A aggression matrix (square matrix)
#' @param eps the epsilon parameter (0 < eps <= 1)
#' @return The transition matrix
#' @examples
#' TransitionMatrix(matrix(c(0, 2, 3, 0), nrow = 2, ncol = 2))
#' @export

TransitionMatrix <- function(A, eps = 1e-12) {
  # Check the input parameters
  if (missing(A)) {
    stop("Please provide an aggression matrix as input.")
  } else {
    if (!is.matrix(A)) {
      stop("A must be a matrix.")
    } # TODO(danm0nster): possibly allow for arrays and data frames
    else {
      rows <- dim(A)[1]
      cols <- dim(A)[2]
      if (rows != cols) {
        stop("Aggression matrix must be square.")
      } else if (dim(A)[1] < 2) {
        stop("Aggression matrix dimension must be greater than 1.")
      } else if (sum(diag(A)) != 0) {
        stop("Aggression matrix has non-zero trace. Self-aggression?")
      } else if (sum(which(A < 0))) {
        stop("Negative element detected in aggression matrix.")
      }
    }
  }
  if (!(is.numeric(eps) || is.integer(eps))) {
    stop("eps must be type numeric or integer")
  } else if (length(eps) != 1) {
    stop("eps must be a scalar.")
  } else if (eps > 1) {
    stop("eps must be less than or equal to 1.")
  } else if (eps < 0) {
    stop("eps must be positive.")
  } else if (eps < .Machine$double.eps) {
    stop("You specified eps less that machine precision.
         Please increase value.")
  }
  # Checks are done, and input should be fine,
  # so we can move on to the actual computations.
  ones <- matrix(1, nrow = rows, ncol = rows)
  ident <- diag(rows)
  numerator <- A + eps * (ones - ident)
  row.sum <- rowSums(A)
  # Repeat this as a column, so each element contains the row sum.
  denominator <- matrix(rep(row.sum, each = cols), ncol = cols, byrow = TRUE)
  denominator <- denominator + (rows - 1) * eps
  return(numerator / denominator)
}
