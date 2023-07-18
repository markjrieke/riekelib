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

#' Covariance Functions
#'
#' @description Generate a \eqn{N \times N} covariance matrix, \eqn{\Sigma}, by
#'   passing a distance vector of length \eqn{N}, `x`, to one of the following
#'   covariance functions:
#'
#'   * **Exponentiated Quadtratic** \deqn{\Sigma_{ij} = \sigma^2 \exp \left( \frac{|x_i - x_j|^2}{-2 \ \rho^2} \right)}
#'   * **Rational Quadratic** \deqn{\Sigma_{ij} = \sigma^2 \left(1 + \frac{|x_i - x_j|^2}{2 \alpha \rho^2} \right)^{-\alpha}}
#'   * **Periodic** \deqn{\Sigma_{ij} = \sigma^2 \exp \left(\frac{-2 \sin^2(\pi |x_i - x_j|/p)}{\rho^2} \right)}
#'   * **Linear** \deqn{\Sigma_{ij} = \beta_0 + \beta (x_i - c)(x_j - c)}
#'   * **White Noise** \deqn{\Sigma_{ij} = \sigma^2 I}
#'
#'  where \eqn{\sigma^2} is the `amplitude` and \eqn{\rho} is the `length_scale`
#'  at which the covariance function can operate.
#'
#'  In the rational quadratic kernel, \eqn{\alpha} is the scale-mixture As
#'  \eqn{\alpha \rightarrow \infty} the rational quadratic converges to the
#'  exponentiated quadratic.
#'
#'  In the periodic kernel, \eqn{p} is the period over which the function repeats.
#'
#'  In the linear kernel, \eqn{\beta_0} and \eqn{\beta} are intercept and slope
#'  parameters, respectively. \eqn{c} is a constant that offsets the linear
#'  covariance from the origin.
#'
#'  Finally, in the white noise kernel, \eqn{I} is the identity matrix.
#'
#' @return A \eqn{N \times N} symmetric, positive-semidefinite covariance matrix
#'
#' @param x A vector containing positions.
#' @param amplitude Vertical scale of the covariance function
#' @param length_scale Horizontal scale of the covariance function
#' @param mixture Scale-mixture for the rational quadratic covariance function
#' @param period Period of repetition for the periodic covariance function
#' @param slope Slope of the linear covariance function
#' @param intercept Intercept of the linear covariance function
#' @param constant A constant that offsets the linear covariance function along
#'  the x-axis from the origin.
#' @param delta A small offset along the diagonal of the resulting covariance
#'  matrix to ensure the function returns a positive-semidefinite matrix. Can
#'  also be used as a white noise kernel to allow for increased variation at
#'  individual positions along the vector `x`.
#'
#' @export
#'
#' @examples
#' x <- seq(from = -2, to = 2, length.out = 50)
#'
#' # heatmap of covariance - higher values = greater covariance
#' cov_exp_quad(x) |> heatmap(Rowv = NA, Colv = NA)
#'
#' # decreasing the length scale decreases the range over which values covary
#' cov_exp_quad(x, length_scale = 0.5) |> heatmap(Rowv = NA, Colv = NA)
#'
#' # rational quadratic includes a mixture parameter
#' cov_rational(x, mixture = 0.1) |> heatmap(Rowv = NA, Colv = NA)
#'
#' # periodic repeats over a distance
#' cov_periodic(x, period = 1, length_scale = 2) |> heatmap(Rowv = NA, Colv = NA)
#'
#' # linear covariance increases/decreases linearly
#' cov_linear(x, slope = 1) |> heatmap(Rowv = NA, Colv = NA)
#'
#' # white noise is applied only along the diagonal
#' cov_noise(x) |> heatmap(Rowv = NA, Colv = NA)
cov_exp_quad <- function(x,
                         amplitude = 1,
                         length_scale = 1,
                         delta = 1e-9) {

  S <- matrix(nrow = length(x), ncol = length(x))

  for (i in 1:nrow(S)) {
    for (j in 1:ncol(S)) {
      S[i,j] <- amplitude*exp(-(0.5/length_scale^2)*(x[i] - x[j])^2)
    }
    S[i,i] <- S[i,i] + delta
  }

  return(S)

}

#' @export
#' @rdname cov_exp_quad
cov_rational <- function(x,
                         amplitude = 1,
                         length_scale = 1,
                         mixture = 1,
                         delta = 1e-9) {

  S <- matrix(nrow = length(x), ncol = length(x))
  for (i in 1:nrow(S)) {
    for (j in 1:ncol(S)) {
      S[i,j] <- amplitude*(1 + ((x[i] - x[j])^2/(2*mixture*length_scale^2)))^(-mixture)
    }
    S[i,i] <- S[i,i] + delta
  }

  return(S)

}

#' @export
#' @rdname cov_exp_quad
cov_periodic <- function(x,
                         amplitude = 1,
                         length_scale = 1,
                         period = 1,
                         delta = 1e-9) {

  S <- matrix(nrow = length(x), ncol = length(x))
  for (i in 1:nrow(S)) {
    for (j in 1:ncol(S)) {
      S[i,j] <- amplitude*exp(-2*(sin(pi*abs(x[i] - x[j])/period)^2)/length_scale^2)
    }
    S[i,i] <- S[i,i] + delta
  }

  return(S)

}

#' @export
#' @rdname cov_exp_quad
cov_linear <- function(x,
                       slope = 0,
                       intercept = 0,
                       constant = 0,
                       delta = 1e-9) {

  S <- matrix(nrow = length(x), ncol = length(x))
  for (i in 1:nrow(S)) {
    for (j in 1:ncol(S)) {
      S[i,j] <- intercept + slope*(x[i] - constant)*(x[j] - constant)
    }
    S[i,i] <- S[i,i] + delta
  }

  return(S)

}

#' @export
#' @rdname cov_exp_quad
cov_noise <- function(x,
                      amplitude = 1) {

  S <- diag(1, nrow = length(x), ncol = length(x))
  S <- amplitude * S
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

