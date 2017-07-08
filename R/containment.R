#' Compute containment measure from transition matrix.
#'
#' Containment is computed as the second largest absolute eigenvalue of the
#' transition matrix.
#'
#' @param trans.mat A transition matrix
#'
#' @return containment
#' @export
#'
#' @examples
#' trans.mat <- array(c(0, 0.5, 0.2, 1, 0, 0.8, 0, 0.5, 0), dim = c(3, 3))
#' containment(trans.mat)
#'
containment <- function(trans.mat) {
  eig.vals <- eigen(trans.mat, only.values = TRUE)
  unique(abs(eig.vals$values))[2]
  #TODO(danm0nster): check that there are at least two unique eigenvalues
}
