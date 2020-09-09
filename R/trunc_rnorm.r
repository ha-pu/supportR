#' @title Truncated normal distribution
#'
#' @description
#' Generate a normal distribtion that is truncated at the lower and/or upper end.
#'
#' @details
#' The function generates a normal distribution for the given parameters
#' (\code{mean}, \code{sd}) and samples \code{n} values that fall within the
#' \code{lwr} and \code{upr} bound. If the distribution does not include
#' \code{n} values within these boundaries the function generates a slightly
#' larger normal distribution from where it samples \code{n} values.
#'
#' @param n number of observations in distribution
#' @param mean mean of distribution
#' @param sd standard deviation of distribution
#' @param lwr lower boundary of distribution
#' @param upr upper boundary of distribution
#' @param nnorm size of normal distribution from where
#' \code{n} values are sampled
#'
#' @return vector of values from generated distribution
#'
#' @examples
#' trunc_rnorm(50, mean = 1, sd = 0.5, lwr = -0.1, upr = 2)
#' @export
#' @importFrom stats rnorm

trunc_rnorm <- function(n, mean = 0, sd = 1, lwr = -Inf, upr = Inf, nnorm = n) {
  message(nnorm)
  samp <- rnorm(n = nnorm, mean = mean, sd = sd)
  samp <- samp[samp >= lwr & samp <= upr]
  if (length(samp) >= n) {
    return(sample(samp, n))
  } else {
    trunc_rnorm(n = n, mean = mean, sd = sd, lwr = lwr, upr = upr, nnorm = nnorm * 1.1)
  }
}
