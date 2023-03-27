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

#' Condition a Gaussian Process
#'
#' @description
#'   `r lifecycle::badge("experimental")`
#'
#'   Condition a Multivariate Normal distribution based on a
#'   Gaussian Process using a exponentiated quadratic covariance matrix.
#'
#' @return A matrix of size \eqn{M \times N}, where \eqn{M} is `length(x_new)`,
#'   and \eqn{N} is `n`. Each column is a random draw from the conditioned
#'   multivariate normal where each row corresponds to the draw at the position
#'   in `x_new`.
#'
#' @param n number of random draws.
#' @param x position of values to condition on.
#' @param y values to condition on (at positions `x`).
#' @param x_new new positions to estimate values at.
#' @inheritParams cov_exp_quad
#'
#' @importFrom assertthat assert_that
#' @importFrom MASS ginv
#' @importFrom MASS mvrnorm
#'
#' @export
#'
#' @examples
#' x <- 1:10
#' y <- rep(0, 10)
#' x_new <- 11:15
#'
#' condition_gaussian_process(3, x, y, x_new)
condition_gaussian_process <- function(n,
                                       x,
                                       y,
                                       x_new,
                                       amplitude = 1,
                                       length_scale = 1,
                                       delta = 1e-9) {

  # check that x/y are same length
  assertthat::assert_that(length(x) == length(y),
                          msg = "`x` and `y` must be the same length.")

  # create x-vector following wikipedia notation
  x1 <- x_new
  x2 <- x
  x_combined <- c(x1, x2)

  # create combined covariance matrix
  S <- cov_exp_quad(x_combined, amplitude, length_scale, delta)

  # separate out covariance matrix
  S11 <- S[1:length(x1), 1:length(x1)]
  S22 <- S[(length(x1) + 1):nrow(S), (length(x1) + 1):ncol(S)]
  S21 <- S[(length(x1) + 1):nrow(S), 1:length(x1)]
  S12 <- t(S21)

  # conditional mean/covariance
  mu_bar <- S12 %*% MASS::ginv(S22) %*% y
  Sigma_bar <- S11 - S12 %*% MASS::ginv(S22) %*% S21

  # simulate new outcomes for y_new
  y_new <- matrix(nrow = length(x_new), ncol = n)
  for (i in 1:n) {
    y_new[,i] <- MASS::mvrnorm(1, mu_bar, Sigma_bar)
  }

  return(y_new)

}

