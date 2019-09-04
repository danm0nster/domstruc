#' Compute containment measure from transition matrix.
#'
#' Containment is computed as the second largest absolute eigenvalue of the
#' transition matrix.
#'
#' @param transition_matrix A transition matrix
#'
#' @return containment
#' @export
#'
#' @examples
#' trans_matrix <- array(c(0, 0.5, 0.2, 1, 0, 0.8, 0, 0.5, 0), dim = c(3, 3))
#' containment(trans_matrix)
containment <- function(transition_matrix) {
  eigen_values <- eigen(transition_matrix, only.values = TRUE)
  unique(abs(eigen_values$values))[2]
  # TODO(danm0nster): check that there are at least two unique eigenvalues
}
