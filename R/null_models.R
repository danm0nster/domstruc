#' Make a downward null model
#'
#' @param aggression_matrix An aggression matrix
#' @param blur The probability that an agent randomly attacks another
#'   agent. If blur > 0 is means that the agent deviates from the pure
#'   downward null heuristic
#' @param epsilon Regularization term for computing eigenvalue centrality in the
#'   model
#'
#' @return A new aggression matrix based on the downward null heuristic model.
#' @export
#'
#' @examples
dom_make_downward_null <- function(aggression_matrix,
                                   blur = 0,
                                   epsilon = 0.694) {
  check_aggression_matrix(aggression_matrix)
  check_probability(blur)
  check_epsilon(epsilon)
  n <- dim(aggression_matrix)[1]
  ec_power <- dom_ec(aggression_matrix, epsilon = epsilon)
  # Higher ec_power is lower power. Higher rank is higher status.
  ranks <- dom_ranks(ec_power)
  # Make pairwise comparisons using the outer product to avoid a loop
  other_below <- outer(ranks[1:n], ranks[1:n], FUN = ">")
  other_above <- !other_below
  # Set the diagonal to false, since agents are neither above
  # or below themselves.
  diag(other_above) <- FALSE
  number_below_agent <- matrix(
    rep(rowSums(other_below), each = n),
    ncol = n,
    byrow = TRUE)
  total_aggression_per_agent <- matrix(
    rep(rowSums(aggression_matrix), each = n),
    ncol = n,
    byrow = TRUE)
  downward_null_matrix <- matrix(0, nrow = n, ncol = n)
  # Distribute aggression to those below with probability (1 - randomness)
  # and distribute the rest among all other agents with probability randomness.
  downward_null_matrix[other_below] <-
    blur * total_aggression_per_agent[other_below] / n +
    (1 - blur) * total_aggression_per_agent[other_below] /
    number_below_agent[other_below]
  downward_null_matrix[other_above] <-
    blur * total_aggression_per_agent[other_above] / n
  return(downward_null_matrix)
}
