#' @title Logit to probability
#'
#' @description
#' The function transforms a logit value to a probability value.
#'
#' @param logit \code{numeric} vector of logit values
#'
#' @return Probability value of the logit value served as input.
#'
#' @references
#' \itemize{
#'   \item \url{https://en.wikipedia.org/wiki/Logit}
#'   \item \url{https://sebastiansauer.github.io/Rcode/logit2prob.R}
#' }
#'
#' @examples
#' data <- create_data()
#' mod <- glm(female_ceo ~ advertising, data = data, family = "binomial")
#' coefs <- coef(mod)
#' logit_to_prob(logit = coefs[["advertising"]])
#' @export

# logit_to_prob ----
logit_to_prob <- function(logit) {
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}
