#' Transform a value on a bound scale to an unbounded scale
#'
#' @param x Bound value to be transformed
#' @param lower_bound Lower bound of x; defaults to 0.
#' @param upper_bound Upper bound of x; defaults to 1.
#' @param base Base used for log-transform, defaults to exp(1) = e
#'
#' @details Transform a value on a bound scale to an unbound scale, i.e.,
#' `[-Inf, Inf]`. Values at the lower/upper bounds will return `-Inf`/`Inf`.
#' Uses the natural base, `e`, by default, but other bases can be passed to
#' the parameter `base`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # unbound vector of values bound between [0, 1]
#' myvec <- seq(0, 10, 1)/10
#' logit(myvec)
#'
#' # unbound vector of values between [100, 200]
#' myvec <- seq(100, 200, 10)
#' logit(myvec, lower_bound = 100, upper_bound = 200)
#' }
logit <- function(x, lower_bound = 0, upper_bound = 1, base = exp(1)) {

  # transform x to be bound by [0, 1]
  x <- (x - lower_bound)/(upper_bound - lower_bound)

  # apply logit transformation
  x <- log(x/(1-x), base = base)

  return(x)

}

#' Transform a value on an unbounded scale to a bound scale
#'
#' @param x Unbounded value to be transformed
#' @param lower_bound Lower bound of new scale; defaults to 0.
#' @param upper_bound Upper bound of new scale; defaults to 1.
#' @param base Base used for transformation; defaults to exp(1) = e
#'
#' @details Transform a value on an unbounded scale (i.e., `[-Inf, Inf]`) to a
#' bound scale. Passing `-Inf`/`Inf` will return the lower/upper bounds,
#' respectively. Uses the natural base, `e`, by default, but other bases can be
#' passed to the parameter `base`
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # bound vector of unbounded values to [0, 1]
#' myvec <- rnorm(10)
#' expit(myvec)
#'
#' # bound vector of unbounded values to [100, 200]
#' expit(myvec, lower_bound = 100, upper_boun = 200)
#' }
expit <- function(x, lower_bound = 0, upper_bound = 1, base = exp(1)) {

  # bound x to [0, 1]
  x <- 1/(1 + base^(-x))

  # transform to custom bounded scale
  x <- x * (upper_bound - lower_bound) + lower_bound

  return(x)

}

