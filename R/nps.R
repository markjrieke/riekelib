#' Net Promoter Score (NPS) Statistical Tools
#'
#' @description
#' Density, distribution function, quantile function, and random generation of
#' Net Promoter Scores (NPS) defined by counts of `promoters`, `passives`, and
#' `detractors`.
#'
#' @param n number of observations to generate
#' @param promoters,passives,detractors counts of promoters, passives, and detractors
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # generate 10000 nps scores based on a sample of 100 respondents
#' rnps(10000, 70, 20, 10)
#' }
rnps <- function(n,
                 promoters,
                 passives,
                 detractors) {

  nps <- rdirichlet(n, c(promoters, passives, detractors))
  nps <- nps[, 1] - nps[, 3]

  return(nps)

}

#' Produce simulated sample quantiles of NPS
#'
#' @description
#' `qnps()` simulates a sample of NPS scores, then produces sample quantiles
#'   corresponding to the given probabilities.
#'
#' @param p numeric vector of probabilities with values in `[0, 1]`.
#' @param promoters,passives,detractors counts of promoters, passives, and detractors
#' @param sims number of simulated NPS scores to generate and use in the sample
#'   quantile calculation.
#' @param na.rm logical; if true, any `NA` and `NaN`s are removed before the
#'   quantiles are computed.
#' @param names logical; if true, the result has a `names` attribute. Set to
#'   `FALSE` to speedup with many `probs`.
#' @param ... additional params to pass to `stats::quantile()`
#'
#' @export
#'
#' @importFrom stats quantile
#' @examples
#' \dontrun{
#' # estimate the 95% nps quantile range of a sample of 100 respondents
#' qnps(c(0.025, 0.5, 0.975), 70, 20, 10)
#' }
qnps <- function(p,
                 promoters,
                 passives,
                 detractors,
                 sims = 10000,
                 na.rm = FALSE,
                 names = FALSE,
                 ...) {

  # simulate scores
  nps <- rnps(sims, promoters, passives, detractors)

  # quantile
  nps <-
    stats::quantile(
      nps,
      probs = p,
      names = names,
      na.rm = na.rm,
      ...
    )

  return(nps)

}

########################## IN PROGRESS #########################################

#' @export
pnps <- function(q,
                 promoters,
                 passives,
                 detractors,
                 sims = 10000,
                 significant_digits = 3) {

  # set vector length based on significant digits desired
  len <- 10^significant_digits

  # quantile vector - p-value is the index of the vector
  nps <- qnps(seq(0, 1, length.out = len), promoters, passives, detractors, sims = sims)

  # filter based on specified quantile
  nps <- nps[nps <= q]

  # return the length as the p-value
  p <- length(nps)/len

  return(p)

}

#' @export
dnps <- function(x,
                 promoters,
                 passives,
                 detractors,
                 sims = 10000,
                 significant_digits = 3) {

  # set vector length based on significant digits desired
  len <- 10^significant_digits

  # quantile vector - p-value is the index of the vector
  nps <- qnps(seq(0, 1, length.out = len), promoters, passives, detractors, sims = sims)

  # params for calculating derivative
  delta_p <- 1/len
  idx <- length(nps[nps <= x])

  # return dev
  if (idx == len) {

    d <- delta_p/(nps[idx] - nps[idx - 1])

  } else if (idx == 0) {

    d <- delta_p/(nps[2] - nps[1])

  } else {

    d <- delta_p/(nps[idx + 1] - nps[idx])

  }

  return(d)

}

# -----------------------------------internal-----------------------------------


# nps_interval - add l8r dawg

### ALSO ###
# think about adding a section for dirichlet
# since gtools has been orphaned

