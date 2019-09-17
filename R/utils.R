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
#' dom_transition_matrix(matrix(c(0, 2, 3, 0), nrow = 2, ncol = 2))
#' @export

dom_transition_matrix <- function(aggression_matrix, epsilon = 0.694) {
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
    byrow = TRUE
  )
  return(numerator / denominator)
}

#' Resample an aggression matrix
#'
#' @param aggression_matrix The aggression matrix to be resampled
#'
#' @return a new aggression matrix resampled over the probability distribution
#'   obtained from the original aggression matrix
#' @export
#'
#' @examples
dom_resample <- function(aggression_matrix) {
  if (missing(aggression_matrix)) {
    stop("Please provide an aggression matrix as input.")
  } else {
    check_aggression_matrix(aggression_matrix)
  }
  dimension <- dim(aggression_matrix)[1]
  # Construct a matrix that is the probability distribution over all pairs
  num_attacks <- sum(aggression_matrix)
  prob <- aggression_matrix / num_attacks
  # Compute the cumulative probability
  cumulative <- cumsum(prob)
  new_matrix <- matrix(0, nrow = dimension, ncol = dimension)
  # Generate random numbers
  rnds <- stats::runif(num_attacks)
  for (p in rnds) {
    # Find the first element with a cumulative probability larger than p and add
    # an atttack.
    #
    # Alternative: make new_matrix a vector and then reshape to a matrix at the
    # end. That might be faster.
    lin_index <- min(which(cumulative > p))
    r <- ((lin_index - 1) %% dimension) + 1
    c <- floor((lin_index - 1) / dimension) + 1
    new_matrix[r, c] <- new_matrix[r, c] + 1
  }
  return(new_matrix)
}

#' Compute rank focused aggression and number of observation pairs
#'
#' @param aggression_matrix aggression matrix (square matrix)
#' @param epsilon the epsilon parameter (0 < eps <= 1)
#'
#' @return A data.frame with columns delta (rank difference), agg (total
#'   aggression at rank difference delta) and agg_norm (total aggression that
#'   could have been directed at rank difference delta)
#' @export
#'
#' @examples
dom_rank_focused_aggression <- function(aggression_matrix, epsilon = 0.694) {
  check_aggression_matrix(aggression_matrix)
  check_epsilon(epsilon)
  n <- dim(aggression_matrix)[1]
  ec_power <- dom_ec(aggression_matrix, epsilon = epsilon)
  ranks <- dom_ranks(ec_power)
  # count the nummber of aggressions at each rank difference
  delta_rank_ij <- outer(ranks, ranks, FUN = "-")
  delta_agg <- data.frame(delta = unique(as.vector(delta_rank_ij)),
                                agg = 0,
                                agg_norm = 0)
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {
        delta_agg$agg[delta_agg$delta == delta_rank_ij[i, j]] <-
          delta_agg$agg[delta_agg$delta == delta_rank_ij[i, j]] +
          aggression_matrix[i, j]
        delta_agg$agg_norm[delta_agg$delta == delta_rank_ij[i, j]] <-
          delta_agg$agg_norm[delta_agg$delta == delta_rank_ij[i, j]] +
          sum(aggression_matrix[i, ])
      }
    }
  }
  # Remove row with delta == 0
  delta_agg <- delta_agg[delta_agg$delta != 0, ]
  return(delta_agg)
}

#' Get ranks from a vector of power scores
#'
#' @param power A vector of power scores (lower power is higher rank) for each
#'   agent
#'
#' @return A vector of the ranks of each agent
#' @export
#'
#' @examples
dom_ranks <- function(power) {
  # TODO: make some checks on the power (vector of more than 1 real elements)
  # This is tricky: First we sort the indices in decreasing order.
  # Then we sort these in increasing order to get the rank.
  # Higher power is lower rank. Higher rank is higher status.
  return(order(order(power, decreasing = TRUE)))
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
#' Execution will halt if an invalid regularization parameter is found,
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


#' Check that the argument is a valid probability
#'
#' This internal function checks that the argument is a scalar between
#' zero and one.
#'
#' @param p
#'
#' @return
#'
#' @noRd
check_probability <- function(p) {
  if (!(is.numeric(p) || is.integer(p))) {
    stop("A probability must be type numeric or integer")
  } else if (length(p) != 1) {
    stop("A probability must be a scalar.")
  } else if (p > 1) {
    stop("A probability must be less than or equal to 1.")
  } else if (p < 0) {
    stop("A probability must be positive.")
  }
}
