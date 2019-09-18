#' Compute bootstrap estimates of focus and position for the downward null model
#' with confidence intervals
#'
#' @param aggression_matrix An aggression matrix.
#' @param blur_values A vector of blur values to use in the downward null model.
#' @param replications The number of bootstrap replications.
#' @param epsilon The regularization parameter for computing eigenvalue centrality.
#'
#' @return Returns a data frame with columns `blur`, `focus`, `position`
#' @export
#'
#' @examples
dom_make_blur_data <- function(aggression_matrix,
                               blur_values = 0:10/10,
                               replications = 100,
                               epsilon = 0.694) {
  if (missing(aggression_matrix)) {
    stop("Please provide an aggression matrix as input.")
  } else {
    check_aggression_matrix(aggression_matrix)
  }
  check_epsilon(epsilon)
  for (b in blur_values) {
    check_probability(b)
  }
  # TODO: Make a check on replications
  # Loop over blur values, compute position, focus and their CI
  blur_data <- data.frame(blur = blur_values,
                          focus = NA,
                          focus_ci_hi = NA,
                          focus_ci_lo = NA,
                          position = NA,
                          position_ci_hi = NA,
                          position_ci_lo = NA)
  for (b in blur_values) {
    focus_vec <- replicate(replications,
                           dom_focus(
                             dom_resample(
                               dom_make_downward_null(
                                 dom_resample(aggression_matrix),
                                 blur = b,
                                 epsilon = epsilon))))
    position_vec <- replicate(replications,
                              dom_position(
                                dom_resample(
                                  dom_make_downward_null(
                                    dom_resample(aggression_matrix),
                                    blur = b,
                                    epsilon = epsilon))))
    foc_mean <- mean(focus_vec)
    foc_sd <- sd(focus_vec)
    pos_mean <- mean(position_vec)
    pos_sd <- sd(position_vec)
    blur_data$focus[blur_data$blur == b] <- foc_mean
    # FIXME: change these to CI instead of std. dev.
    blur_data$focus_ci_hi[blur_data$blur == b] <- foc_mean + foc_sd
    blur_data$focus_ci_lo[blur_data$blur == b] <- foc_mean - foc_sd
    blur_data$position[blur_data$blur == b] <- pos_mean
    blur_data$position_ci_hi[blur_data$blur == b] <- pos_mean + pos_sd
    blur_data$position_ci_lo[blur_data$blur == b] <- pos_mean - pos_sd
  }
  return(blur_data)
}
