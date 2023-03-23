#' The Cholesky Decomposition
#'
#' @description Compute the Cholesky factorization of a real, symmetric,
#'   positive-definite square matrix. Returns the lower-left triangular matrix
#'   to match Stan's version. This function is simply an opinionated wrapper of
#'   the base R function, `chol()`.
#'
#' @return A lower-triangular square matrix the same size as `x`.
#'
#' @param x A symmetric, positive-definite square matrix.
#' @param ... Additional parameters to pass to `chol()`.
#'
#' @export
#'
#' @examples
#' x <- matrix(c(2, 1, 1, 2), nrow = 2)
#' cholesky_decompose(x)
cholesky_decompose <- function(x, ...) {

  t(chol(x, ...))

}

#' Exponentiated Quadratic Covariance
#'
#' @description Generate a \eqn{N \times N} covariance matrix, \eqn{\Sigma}, by
#'   passing a distance vector `x` to the exponentiated quadratic kernel function:
#'
#'   \deqn{\Sigma_{ij} = \alpha^2 \exp \left( \frac{|x_i - x_j|^2}{-2 \ \rho} \right) + \delta_{ij} \ \sigma}
#'
#'  where \eqn{\alpha^2} is the **amplitude**, \eqn{\rho} is the
#'  **length_scale**, and \eqn{\sigma} is a small offset along the diagonal to
#'  allow for additional variation.
#'
#' @return A \eqn{N \times N} symmetric, positive-semidefinite covariance matrix
#'
#' @param x A vector containing positions
#' @param amplitude Numeric, controls the vertical scale of the covariance function
#' @param length_scale Numeric, controls the horizontal scale of the covariance function
#' @param delta A small offset added along the diagonal to ensure that the
#'   function returns a positive-semidefinite matrix. Larger values allow for
#'   increased variation at individual positions along the vector `x`.
#'
#' @export
#'
#' @examples
#' x <- 1:12
#' cov_exp_quad(x)
cov_exp_quad <- function(x,
                         amplitude = 1,
                         length_scale = 1,
                         delta = 1e-9) {

  S <- matrix(nrow = length(x), ncol = length(x))

  for (i in 1:nrow(S)) {
    for (j in 1:ncol(S)) {
      S[i,j] <- amplitude*exp(-(0.5/length_scale)*(x[i] - x[j])^2)
    }
    S[i,i] <- S[i,i] + delta
  }

  return(S)

}
