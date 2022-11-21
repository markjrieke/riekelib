#' Transform a between bound and unbounded scales.
#'
#' @param x Value to be rescaled.
#' @param lower_bound Lower bound of x; defaults to 0.
#' @param upper_bound Upper bound of x; defaults to 1.
#' @param base Base used for log-transform, defaults to `exp(1)` = e
#'
#' @details `logit()` will transform a value on a bound scale to an unbound scale,
#' while `expit()` performs the reverse transformation. Values at the lower/upper
#' bounds correspond to `-Inf`/`Inf` on the unbounded scale. Uses the natural base,
#' `e`, by default, but other bases can be passed to the parameter `base`.
#'
#' @export
#'
#' @examples
#' # unbound vector of values bound between [0, 1]
#' myvec <- seq(0, 10, 1)/10
#' logit(myvec)
#'
#' # unbound vector of values between [100, 200]
#' myvec <- seq(100, 200, 10)
#' logit(myvec, lower_bound = 100, upper_bound = 200)
#'
#' # bound vector of unbounded values to [0, 1]
#' myvec <- rnorm(10)
#' expit(myvec)
#'
#' # bound vector of unbounded values to [100, 200]
#' expit(myvec, lower_bound = 100, upper_bound = 200)
logit <- function(x, lower_bound = 0, upper_bound = 1, base = exp(1)) {

  # transform x to be bound by [0, 1]
  x <- (x - lower_bound)/(upper_bound - lower_bound)

  # apply logit transformation
  x <- log(x/(1-x), base = base)

  return(x)

}

#' @export
#' @rdname logit
expit <- function(x, lower_bound = 0, upper_bound = 1, base = exp(1)) {

  # bound x to [0, 1]
  x <- 1/(1 + base^(-x))

  # transform to custom bounded scale
  x <- x * (upper_bound - lower_bound) + lower_bound

  return(x)

}

