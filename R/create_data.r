#' @title
#' Create data
#'
#' @description
#' Generate simulated dataset that can be used for testing and reproducible
#' example.
#'
#' @details
#' The base for the data simulation is fundamental data for a large number of
#' U.S. firms. For each variable, values are simulated based on a process similar
#' to the data for these US firms. Yet the function simulates each variable
#' individually, so relationships observed in the data are simulated by
#' \code{create_data}.
#'
#' The function \code{create_data} generates a \code{data.frame} with \code{n}
#' rows (defaults to 1,000) and the following 11 columns:
#'  \itemize{
#'    \item \code{advertising} [\code{double}] - simulated as
#'    \code{trunc_rnorm(n = n, mean = 97, sd = 326, lwr = 0)}
#'    \item \code{assets} [\code{double}] - simulated as
#'    \code{trunc_rnorm(n = n, mean = 14589, sd = 98124, lwr = 0)}
#'    \item \code{country} [\code{character}] - simulated as
#'    \code{sample(LETTERS[1:4], size = n, replace = TRUE)}
#'    \item \code{female_ceo} [\code{logical}] - simulated as
#'    \code{as.logical(rbinom(n = n, size = 1, prob = 0.1))}
#'    \item \code{international} [\code{logical}] - simulated as
#'    \code{as.logical(rbinom(n = n, size = 1, prob = 0.1))}
#'    \item \code{revenue} [\code{double}] - simulated as
#'    \code{trunc_rnorm(n = n, mean = 3796, sd = 15420, lwr = 0)}
#'    \item \code{rnd} [\code{double}] - simulated as
#'    \code{trunc_rnorm(n = n, mean = 200, sd = 752, lwr = 0)}
#'    \item \code{sic} [\code{factor}] - simulated as
#'    \code{as.factor(sample(0:9, size = n, replace = TRUE, prob =
#'    c(0.05, 0.1, 0.1, 0.1, 0.05, 0.05, 0.35, 0.1, 0.05, 0.05)))}
#'    \item \code{staff} [\code{double}] - simulated as
#'    \code{trunc_rnorm(n = n, mean = 649, sd = 2145, lwr = 0)}
#'    \item \code{stock_ownership} [\code{character}] - simulated as
#'    \code{sample(LETTERS[1:4], size = n, replace = TRUE)}
#'    \item \code{ebit} [\code{double}] - simulated as the result
#'    of the other variables, using and intercept of -296 and and a
#'    random error term (\code{rnorm(n = n, mean = 0, sd = 500)})
#'  }
#'
#' @seealso \code{\link[supportR]{trunc_rnorm}}
#'
#' @param n Number of observations to be simulated.
#'
#' @return \code{data.frame} or \code{tibble} (when package \code{tibble} is
#' available) with \code{n} rows and 11 columns.
#'
#' @examples
#' create_data()
#' create_data(n = 50)
#' @export
#' @importFrom stats rbinom
#' @importFrom stats rnorm
#' @importFrom stats sd

# create_data ----
create_data <- function(n = 1000) {
  data_value <- data.frame(
    advertising = trunc_rnorm(n = n, mean = 97, sd = 326, lwr = 0),
    assets = trunc_rnorm(n = n, mean = 14589, sd = 98124, lwr = 0),
    country = sample(LETTERS[1:4], size = n, replace = TRUE),
    female_ceo = as.logical(rbinom(n = n, size = 1, prob = 0.1)),
    international = as.logical(rbinom(n = n, size = 1, prob = 0.1)),
    revenue = trunc_rnorm(n = n, mean = 3796, sd = 15420, lwr = 0),
    rnd = trunc_rnorm(n = n, mean = 200, sd = 752, lwr = 0),
    sic = as.factor(sample(0:9, size = n, replace = TRUE, prob = c(0.05, 0.1, 0.1, 0.1, 0.05, 0.05, 0.35, 0.1, 0.05, 0.05))),
    staff = trunc_rnorm(n = n, mean = 649, sd = 2145, lwr = 0),
    stock_ownership = sample(LETTERS[1:4], size = n, replace = TRUE)
  )

  data_value$ebit <- -296 + rnorm(n = n, mean = 0, sd = 500)
  data_value$ebit <- data_value$ebit + data_value$female_ceo * -66
  data_value$ebit <- data_value$ebit + data_value$assets * 0.02
  data_value$ebit <- data_value$ebit + data_value$revenue * 0.06
  data_value$ebit <- data_value$ebit + data_value$advertising * 0.77
  data_value$ebit <- data_value$ebit + data_value$staff * -0.36
  data_value$ebit <- data_value$ebit + data_value$rnd * 1.19
  data_value$ebit <- data_value$ebit + data_value$international * 293

  data_value$ebit[data_value$country == "B"] <- data_value$ebit[data_value$country == "B"] + 28
  data_value$ebit[data_value$country == "C"] <- data_value$ebit[data_value$country == "C"] + 19
  data_value$ebit[data_value$country == "D"] <- data_value$ebit[data_value$country == "D"] - 9

  data_value$ebit[data_value$stock_ownership == "B"] <- data_value$ebit[data_value$stock_ownership == "B"] - 53
  data_value$ebit[data_value$stock_ownership == "C"] <- data_value$ebit[data_value$stock_ownership == "C"] - 41
  data_value$ebit[data_value$stock_ownership == "D"] <- data_value$ebit[data_value$stock_ownership == "D"] - 73

  data_value$ebit[data_value$sic == 1] <- data_value$ebit[data_value$sic == 1] + 175
  data_value$ebit[data_value$sic == 2] <- data_value$ebit[data_value$sic == 2] + 341
  data_value$ebit[data_value$sic == 3] <- data_value$ebit[data_value$sic == 3] + 320
  data_value$ebit[data_value$sic == 4] <- data_value$ebit[data_value$sic == 4] + 729
  data_value$ebit[data_value$sic == 5] <- data_value$ebit[data_value$sic == 5] - 133
  data_value$ebit[data_value$sic == 6] <- data_value$ebit[data_value$sic == 6] + 219
  data_value$ebit[data_value$sic == 7] <- data_value$ebit[data_value$sic == 7] + 369
  data_value$ebit[data_value$sic == 8] <- data_value$ebit[data_value$sic == 8] + 593
  data_value$ebit[data_value$sic == 9] <- data_value$ebit[data_value$sic == 9] + 191

  if (requireNamespace("tibble", quietly = TRUE)) {
    data_value <- tibble::as_tibble(data_value)
  }

  return(data_value)
}
