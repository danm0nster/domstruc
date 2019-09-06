#' Compute the transition matrix.
#'
#' The transition matrix is computed from the aggression matrix.
#' The element T[i,j] of the transition matrix is the probability
#' that individual i attacks individual j.
#'
#' @param aggression_matrix aggression matrix (square matrix)
#' @param epsilon the epsilon parameter (0 < eps <= 1)
#' @return The transition matrix
#' @examples
#' transition_matrix(matrix(c(0, 2, 3, 0), nrow = 2, ncol = 2))
#' @export

transition_matrix <- function(aggression_matrix, epsilon = 0.694) {
  # Check the input parameters
  if (missing(aggression_matrix)) {
    stop("Please provide an aggression matrix as input.")
  } else {
    # TODO(danm0nster): possibly allow for arrays and data frames
    check_aggression_matrix(aggression_matrix)
  }
  check_epsilon(epsilon)
  # Checks are done, and input should be fine,
  # so we can move on to the actual computations.
  num_rows <- dim(aggression_matrix)[1]
  ones <- matrix(1, nrow = num_rows, ncol = num_rows)
  ident <- diag(num_rows)
  numerator <- aggression_matrix + epsilon * (ones - ident)
  # Compute row sums and normalize by these, so probabilities sum to 1
  row.sum <- rowSums(numerator)
  # Repeat this as a column, so each element contains the row sum.
  denominator <- matrix(rep(row.sum, each = num_rows),
                        ncol = num_rows,
                        byrow = TRUE)
  return(numerator / denominator)
}


#' Check if matrix is a valid aggression matrix
#'
#' This is an internal utility function, not exported via NAMESPACE.
#' Execution will halt if an invalid aggression matrix is found,
#' and an error message will be printed.
#'
#' @param aggression_matrix The aggression matrix to be checked
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

#' Check that epsilon is a valid regularization parameter
#'
#' This is an internal utility function, not exported via NAMESPACE.
#' Execution will halt if an invalid aggression matrix is found,
#' and an error message will be printed.
#'
#' @param epsilon
#'
#' @return
#'
#' @noRd
check_epsilon <- function(epsilon) {
  if (!(is.numeric(epsilon) || is.integer(epsilon))) {
    stop("eps must be type numeric or integer")
  } else if (length(epsilon) != 1) {
    stop("eps must be a scalar.")
  } else if (epsilon > 1) {
    stop("eps must be less than or equal to 1.")
  } else if (epsilon < 0) {
    stop("eps must be positive.")
  } else if (epsilon < .Machine$double.eps) {
    error_message <- paste0(
      "You specified eps less that machine precision.\n",
      "Please increase value."
    )
    stop(error_message)
  }
}
