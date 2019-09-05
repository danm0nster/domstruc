#' Compute the transition matrix.
#'
#' The transition matrix is computed from the aggression matrix.
#' The element T[i,j] of the transition matrix is the probability
#' that individual i attacks individual j.
#'
#' @param aggression_matrix aggression matrix (square matrix)
#' @param eps the epsilon parameter (0 < eps <= 1)
#' @return The transition matrix
#' @examples
#' transition_matrix(matrix(c(0, 2, 3, 0), nrow = 2, ncol = 2))
#' @export

transition_matrix <- function(aggression_matrix, eps = 0.694) {
  # Check the input parameters
  if (missing(aggression_matrix)) {
    stop("Please provide an aggression matrix as input.")
  } else {
    # TODO(danm0nster): possibly allow for arrays and data frames
    check_aggression_matrix(aggression_matrix)
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
    error_message <- paste0(
      "You specified eps less that machine precision.\n",
      "Please increase value."
    )
    stop(error_message)
  }
  # Checks are done, and input should be fine,
  # so we can move on to the actual computations.
  num_rows <- dim(aggression_matrix)[1]
  ones <- matrix(1, nrow = num_rows, ncol = num_rows)
  ident <- diag(num_rows)
  numerator <- aggression_matrix + eps * (ones - ident)
  row.sum <- rowSums(aggression_matrix)
  # Repeat this as a column, so each element contains the row sum.
  denominator <- matrix(rep(row.sum, each = num_rows),
                        ncol = num_rows,
                        byrow = TRUE)
  denominator <- denominator + (num_rows - 1) * eps
  return(numerator / denominator)
}


#' Check if matrix is a valid aggression matrix
#'
#' This is an internal utility function, not exported via NAMESPACE.
#'
#' @param aggression_matrix The aggression matrix to be checked
#'
#' @return
#'
#' @examples
#'
#' @noRd
check_aggression_matrix <- function(aggression_matrix) {
  if (!is.matrix(aggression_matrix)) {
    stop("Argument must be a matrix.")
  }
  else {
    num_rows <- dim(aggression_matrix)[1]
    num_cols <- dim(aggression_matrix)[2]
    if (num_rows != num_cols) {
      stop("Aggression matrix must be square.")
    } else if (dim(aggression_matrix)[1] < 2) {
      stop("Aggression matrix dimension must be greater than 1.")
    } else if (sum(diag(aggression_matrix)) != 0) {
      stop("Aggression matrix has non-zero trace. Self-aggression?")
    } else if (sum(which(aggression_matrix < 0))) {
      stop("Negative element detected in aggression matrix.")
    }
  }
}
