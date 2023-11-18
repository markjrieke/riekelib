#' Apply a confidence interval to values in a dataframe based on the beta distribution.
#'
#' @param .data Data frame; a confidence interval will be applied to each row in
#' the data frame.
#' @param alpha First shape parameter of the beta distribution, must be greater than 0
#' @param beta Second shape parameter of the beta distribution, must be greater than 0
#' @param conf Confidence interval, must be between `[0, 1]`. Defaults to 0.95.
#'
#' @family interval verbs
#'
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom stats qbeta
#'
#' @examples
#' # create example df
#' alpha <- rnorm(10, 100, 20)
#' beta <- rnorm(10, 80, 10)
#' example <- dplyr::bind_cols(alpha, beta)
#' colnames(example) <- c("alpha", "beta")
#'
#' # apply the default confidence interval of 0.95
#' beta_interval(example, alpha, beta)
#'
#' # apply a custom confidence interval
#' beta_interval(example, alpha, beta, conf = 0.99)
beta_interval <- function(.data, alpha, beta, conf = 0.95) {

  dplyr::mutate(
    .data,
    ci_lower = stats::qbeta((1 - {{ conf }})/2, {{ alpha }}, {{ beta }}),
    ci_upper = stats::qbeta((1 - {{ conf }})/2 + {{ conf }}, {{ alpha }}, {{ beta }})
  )

}

#' Apply a confidence interval to values in a dataframe based on the normal distribution.
#'
#' @param .data Data frame; a confidence interval will be applied to each row in
#' the data frame.
#' @param mean Mean of the distribution
#' @param std_dev Standard deviation of the distribution
#' @param conf Confidence interval, must be between `[0, 1]`. Defaults to 0.95.
#'
#' @family interval verbs
#'
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom stats qnorm
#'
#' @examples
#' # create example df
#' mean <- rnorm(10, 100, 20)
#' std_dev <- rnorm(10, 80, 10)
#' example <- dplyr::bind_cols(mean, std_dev)
#' colnames(example) <- c("mean", "std_dev")
#'
#' # apply the default confidence interval of 0.95
#' normal_interval(example, mean, std_dev)
#'
#' # apply a custom confidence interval
#' normal_interval(example, mean, std_dev, conf = 0.99)
normal_interval <- function(.data, mean, std_dev, conf = 0.95) {

  dplyr::mutate(
    .data,
    ci_lower = stats::qnorm((1 - {{ conf }})/2, {{ mean }}, {{ std_dev }}),
    ci_upper = stats::qnorm((1 - {{ conf }})/2 + {{ conf }}, {{ mean }}, {{ std_dev }})
  )

}

#' Center and scale a set of values.
#'
#' @description
#' Transform values in `x` such that the mean is approximately 0 and the standard
#' deviation is approximately 1. Note that this is a whole-cloth reimplemntation
#' of a function of the same name from the [rethinking](https://github.com/rmcelreath/rethinking/tree/master)
#' package by [Dr. Richard McElreath](https://xcelab.net/rm/).
#'
#' @param x A vector of values to be scaled.
#'
#' @export
#'
#' @examples
#' x <- rnorm(1000, mean = 3, sd = 4)
#' z <- standardize(x)
#'
#' mean(z)
#' sd(z)
standardize <- function(x) {
  x <- scale(x)
  z <- as.numeric(x)
  attr(z,"scaled:center") <- attr(x,"scaled:center")
  attr(z,"scaled:scale") <- attr(x,"scaled:scale")
  return(z)
}




