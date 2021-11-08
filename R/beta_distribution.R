#' Apply a confidence interval to values in a dataframe based on the beta distribution.
#'
#' @param .data Data frame; a confidence interval will be applied to each row in
#' the data frame.
#' @param alpha First shape parameter of the beta distribution, must be greater than 0
#' @param beta Second shape parameter of the beta distribution, must be greater than 0
#' @param conf Confidence interval, must be between `[0, 1]`. Defaults to 0.95.
#'
#' @importFrom
#'
#' @export
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
    ci_lower = beta_lower(alpha, beta, conf),
    ci_upper = beta_upper(alpha, beta, conf)
  )

}

#' Util function for finding the lower bound of a confidence interval on a beta distribution
#'
#' @param alpha First shape parameter of the beta distribution, must be greater than 0
#' @param beta Second shape parameter of the beta distribution, must be greater than 0
#' @param conf Confidence interval, must be between `[0, 1]`.
beta_lower <- function(alpha, beta, conf) {

  stats::qbeta((1-conf)/2, alpha, beta)

}

#' Util function for finding the upper bound of a confidence interval on a beta distribution
#'
#' @param alpha First shape parameter of the beta distribution, must be greater than 0
#' @param beta Second shape parameter of the beta distribution, must be greater than 0
#' @param conf Confidence interval, must be between [0, 1].
beta_upper <- function(alpha, beta, conf) {

  stats::qbeta((1-conf)/2+conf, alpha, beta)

}


