#' Compute bootstrap estimates of focus and position for the downward null model
#' with confidence intervals
#'
#' @param aggression_matrix An aggression matrix.
#' @param blur_values A vector of blur values to use in the downward null model.
#' @param replications The number of bootstrap replications.
#' @param epsilon The regularization parameter for computing eigenvalue
#'   centrality.
#' @param conf_level The confidence level used to compute lower and upper limits
#'   of the confidence intervals on focus and position. Default is 0.95.
#'
#' @return Returns a data frame with columns `blur`, `focus`, `position`
#'   and confidence intervals on focus (`focus_ci_lo`, `focus_ci_hi`) and
#'   position (`position_ci_lo`, `position_ci_hi`).
#' @export
#'
#' @examples
dom_make_blur_data <- function(aggression_matrix,
                               blur_values = 0:10 / 10,
                               replications = 100,
                               epsilon = 0.694,
                               conf_level = 0.95) {
  if (missing(aggression_matrix)) {
    stop("Please provide an aggression matrix as input.")
  } else {
    check_aggression_matrix(aggression_matrix)
  }
  check_epsilon(epsilon)
  for (b in blur_values) {
    check_probability(b)
  }
  check_probability(conf_level)
  # TODO: Make a check on replications
  blur_data <- data.frame(blur = blur_values,
                          focus = NA,
                          focus_ci_hi = NA,
                          focus_ci_lo = NA,
                          position = NA,
                          position_ci_hi = NA,
                          position_ci_lo = NA)
  # Loop over blur values, compute position, focus and their CI
  for (b in blur_values) {
    blur_matrix <- dom_make_downward_null(aggression_matrix,
                                          blur = b,
                                          epsilon = epsilon)
    focus_vec <- replicate(replications,
                           dom_focus(
                             dom_resample(blur_matrix)))
    position_vec <- replicate(replications,
                              dom_position(
                                dom_resample(blur_matrix)))
    # Get a downward null aggression matrix for the blur level and compute
    # focus and position from this to be able to estimate the bootstrap bias.
    null_aggression_matrix <- dom_make_downward_null(aggression_matrix, blur = b, epsilon = epsilon)

    focus_raw <- dom_focus(null_aggression_matrix, epsilon = epsilon)
    focus_ci <- bootstrap_ci(focus_vec, focus_raw,
                             conf_level = conf_level,
                             correction = FALSE,
                             recenter = FALSE)
    position_raw <- dom_position(null_aggression_matrix, epsilon = epsilon)
    position_ci <- bootstrap_ci(position_vec, position_raw,
                                conf_level = conf_level,
                                correction = FALSE,
                                recenter = FALSE)

    blur_data$focus[blur_data$blur == b] <- focus_ci["mean"]
    blur_data$focus_ci_hi[blur_data$blur == b] <- focus_ci["high"]
    blur_data$focus_ci_lo[blur_data$blur == b] <- focus_ci["low"]
    blur_data$position[blur_data$blur == b] <- position_ci["mean"]
    blur_data$position_ci_hi[blur_data$blur == b] <- position_ci["high"]
    blur_data$position_ci_lo[blur_data$blur == b] <- position_ci["low"]
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
                          epsilon = 0.694,
                          conf_level = 0.95) {
  if (missing(aggression_matrix)) {
    stop("Please provide an aggression matrix as input.")
  } else {
    check_aggression_matrix(aggression_matrix)
  }
  check_epsilon(epsilon)
  check_probability(conf_level)
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
  focus_ci <- bootstrap_ci(focus_vec, focus_raw, correction = FALSE,
                           recenter = TRUE, conf_level = conf_level)

  # Correct position in the same way
  position_raw <- dom_position(aggression_matrix, epsilon = epsilon)
  position_ci <- bootstrap_ci(position_vec, position_raw, correction = FALSE,
                              recenter = TRUE, conf_level = conf_level)

  data$focus <- focus_ci["mean"]
  data$focus_ci_hi <- focus_ci["high"]
  data$focus_ci_lo <- focus_ci["low"]

  data$position <- position_ci["mean"]
  data$position_ci_hi <- position_ci["high"]
  data$position_ci_lo <- position_ci["low"]
  return(data)
}

#' Compute bootstrap confidence intervals and correct for bootstrap bias
#'
#' @param bs_values A vector of bootstrap estimates to be corrected
#' @param raw_mean The "raw" mean of the measure to be corrected, i.e., not the
#'   bootsrapped estimate of the mean.
#' @param conf_level The confidence level. Default is 0.95, i.e. a 95%
#'   confidence interval will be computed.
#' @param correction Control whether to perform bias correction (TRUE) or not
#'   (FALSE).
#'
#' @return named vector with corrected "mean", "high" (upper CI limit), "low"
#'   (lower CI limit)
#'
bootstrap_ci <- function(bs_values, raw_mean, conf_level = 0.95,
                         correction = FALSE, recenter = FALSE) {
  # Find the bootstrap estimate of the mean
  bs_mean <- mean(bs_values)
  # Find the percentiles of the bootstrap data corresponding to the confidence level
  percentiles <- quantile(bs_values,
                          probs = c(
                            (1 - conf_level) / 2,
                            1 - (1 - conf_level) / 2))
  lo <- percentiles[[1]]
  hi <- percentiles[[2]]
  # Compute the lower and upper values of the CI.
  # Note that lo is used for the upper end of the CI because of the sign and
  # vice versa for hi and the lower end of the CI.
  if (correction) {
    # Bootstrap estimate of the bias
    bs_bias <-  raw_mean - bs_mean
    # Correct for bias
    corrected_mean <- raw_mean + bs_bias
    ci_hi <- raw_mean + (raw_mean - lo)
    ci_lo <- raw_mean + (raw_mean - hi)
  } else {
    if (recenter) {
      corrected_mean <- raw_mean
      # No bias correction, but re-centering CI around raw mean
      ci_hi <- hi + raw_mean - bs_mean
      ci_lo <- lo + raw_mean - bs_mean
    } else {
      corrected_mean <- bs_mean
      # No bias correction, no re-centering
      ci_hi <- hi
      ci_lo <- lo
    }
  }
  return(c(mean = corrected_mean,
           high = ci_hi,
           low = ci_lo))
}
