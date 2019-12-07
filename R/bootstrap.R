#' Compute bootstrap estimates of focus and position for the downward null model
#' with confidence intervals
#'
#' @param aggression_matrix An aggression matrix.
#' @param blur_values A vector of blur values to use in the downward null model.
#' @param replications The number of bootstrap replications.
#' @param epsilon The regularization parameter for computing eigenvalue
#'   centrality.
#'
#' @return Returns a data frame with columns `blur`, `focus`, `position`
#' @export
#'
#' @examples
dom_make_blur_data <- function(aggression_matrix,
                               blur_values = 0:10 / 10,
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
    # TODO: Correct for bootstrap bias
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


#' Compute bootstrap estimates of focus and position for aggression matrix
#'
#' @param aggression_matrix An aggression matrix.
#' @param replications The number of bootstrap replications.
#' @param epsilon The regularization parameter for computing eigenvalue
#'   centrality.
#'
#' @return
#' @export
#'
#' @examples
dom_make_data <- function(aggression_matrix,
                          replications = 100,
                          epsilon = 0.694) {
  # Set up a data frame to hold the results
  data <- data.frame(focus = 0,
                     focus_ci_hi = 0,
                     focus_ci_lo = 0,
                     position = 0,
                     position_ci_hi = 0,
                     position_ci_lo = 0)
  focus_vec <- replicate(replications,
                         dom_focus(
                               dom_resample(aggression_matrix),
                               epsilon = epsilon))
  position_vec <- replicate(replications,
                            dom_position(
                                  dom_resample(aggression_matrix),
                                  epsilon = epsilon))
  # Compute the focus of the un-resampled aggression matrix to correct for
  # bootstrap bias
  focus_raw <- dom_focus(aggression_matrix, epsilon = epsilon)
  bias_corrected_focus <- correct_bootstrap_bias(focus_vec, focus_raw)

  # Correct position in the same way
  pos_mean <- mean(position_vec)
  pos_sd <- sd(position_vec)
  position_raw <- dom_position(aggression_matrix, epsilon = epsilon)
  position_bias <- position_raw - pos_mean
  position_estimate <- position_raw + position_bias
  position_sd_estimate <- sd(position_vec + position_bias)
  bias_corrected_position <- correct_bootstrap_bias(position_vec, position_raw)

  # FIXME: change these to CI instead of std. dev.
  data$focus <- bias_corrected_focus["mean"]
  data$focus_ci_hi <- bias_corrected_focus["high"]
  data$focus_ci_lo <- bias_corrected_focus["low"]

  data$position <- bias_corrected_position["mean"]
  data$position_ci_hi <- bias_corrected_position["high"]
  data$position_ci_lo <- bias_corrected_position["low"]
  return(data)
}

#' Correct for bootstrap bias in mean and hi and low estimates
#'
#' @param bs_values A vector of bootstrap estimates to be corrected
#' @param raw_mean The "raw" mean of the measure to be corrected, i.e., not the
#'   bootsrapped estimate of the mean.
#'
#' @return named vector with corrected "mean", "high" (upper CI limit), "low"
#'   (lower CI limit)
#'
correct_bootstrap_bias <- function(bs_values, raw_mean) {
  # Find the bootstrap estimate the mean
  bs_mean <- mean(bs_values)
  # Bootstrap estimate of the bias
  bs_bias <-  raw_mean - bs_mean
  # Correct for bias
  corrected_mean <- raw_mean + bs_bias
  corrected_sd_estimate <- sd(bs_values + bs_bias)
  return(c(mean = corrected_mean,
           high = corrected_mean + corrected_sd_estimate,
           low = corrected_mean - corrected_sd_estimate))
}
