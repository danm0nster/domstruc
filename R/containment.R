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
containment <- function(trans.mat) {
  eig.vals <- eigen(trans.mat, only.values = TRUE)
  unique(abs(eig.vals$values))[2]
}
