#' Apply a confidence interval to values in a dataframe based on the beta distribution.
#'
#' @param .data Data frame; a confidence interval will be applied to each row in
#' the data frame.
#' @param alpha First shape parameter of the beta distribution, must be greater than 0
#' @param beta Second shape parameter of the beta distribution, must be greater than 0
#' @param conf Confidence interval, must be between `[0, 1]`. Defaults to 0.95.
#'
#' @export
#'
#' @importFrom dplyr mutate
#'
#' @examples
#' \dontrun{
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
#'
#' }
#'
beta_interval <- function(.data, alpha, beta, conf = 0.95) {

  dplyr::mutate(
    .data,
    ci_lower = beta_lower({{alpha}}, {{beta}}, {{conf}}),
    ci_upper = beta_upper({{alpha}}, {{beta}}, {{conf}})
  )

}

#' Util function for finding the lower bound of a confidence interval on a beta distribution
#'
#' @param alpha First shape parameter of the beta distribution, must be greater than 0
#' @param beta Second shape parameter of the beta distribution, must be greater than 0
#' @param conf Confidence interval, must be between `[0, 1]`.
#'
#' @importFrom stats qbeta
#'
beta_lower <- function(alpha, beta, conf) {

  stats::qbeta((1-conf)/2, alpha, beta)

}

#' Util function for finding the upper bound of a confidence interval on a beta distribution
#'
#' @param alpha First shape parameter of the beta distribution, must be greater than 0
#' @param beta Second shape parameter of the beta distribution, must be greater than 0
#' @param conf Confidence interval, must be between `[0, 1]`.
#'
#' @importFrom stats qbeta
#'
beta_upper <- function(alpha, beta, conf) {

  stats::qbeta((1-conf)/2+conf, alpha, beta)

}

#' Apply a confidence interval to values in a dataframe based on the normal distribution.
#'
#' @param .data Data frame; a confidence interval will be applied to each row in
#' the data frame.
#' @param mean Mean of the distribution
#' @param std_dev Standard deviation of the distribution
#' @param conf Confidence interval, must be between `[0, 1]`. Defaults to 0.95.
#'
#' @export
#'
#' @importFrom dplyr mutate
#'
#' @examples
#' \dontrun{
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
#'
#' }
#'
normal_interval <- function(.data, mean, std_dev, conf = 0.95) {

  dplyr::mutate(
    .data,
    ci_lower = normal_lower({{mean}}, {{std_dev}}, {{conf}}),
    ci_upper = normal_upper({{mean}}, {{std_dev}}, {{conf}})
  )

}

#' Util function for finding the lower bound of a confidence interval on a normal distribution
#'
#' @param mean Mean of the distribution
#' @param std_dev Standard deviation of the distribution
#' @param conf Confidence interval, must be between `[0, 1]`
#'
#' @importFrom stats qnorm
#'
normal_lower <- function(mean, std_dev, conf) {

  stats::qnorm((1-conf)/2, mean, std_dev)

}

#' Util function for finding the upper bound of a confidence interval on a normal distribution
#'
#' @param mean Mean of the distribution
#' @param std_dev Standard deviation of the distribution
#' @param conf Confidence interval, must be between `[0, 1]`
#'
#' @importFrom stats qnorm
#'
normal_upper <- function(mean, std_dev, conf) {

  stats::qnorm((1-conf)/2+conf, mean, std_dev)

}


#' Calculate the percent difference between two values
#'
#' @param x,y Values you want to determine the percent difference between.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' pct_diff(182, 195)
#' }
pct_diff <- function(x, y) {

  abs(x - y)/mean(c(x, y))

}


#' Calculate the effect size for a Chi-Square test using the Cramer's V measure
#'
#' @param chi_sq Chi-Square statistic from a Chi-Square test
#' @param n total number of observations
#' @param deg_free degrees of freedom in the Chi-Square test
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cramer_v(33.2, 125, 4)
#' }
cramer_v <- function(chi_sq, n, deg_free) {

  sqrt(chi_sq/(n * deg_free))

}




