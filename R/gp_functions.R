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
