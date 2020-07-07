\name{logit_to_prob}

\alias{logit_to_prob}

\title{Logit to probability}

\description{
  The function transforms a logit value to a probability value.
}

\usage{
  logit_to_prob(logit)
}


\arguments{
  \item{logit}{\code{numeric} vector of logit values}
}

\value{
  Probability value of the logit value served as input.
}

\references{
  \itemize{
    \item{\url{https://en.wikipedia.org/wiki/Logit}}
    \item{\url{https://sebastiansauer.github.io/Rcode/logit2prob.R}}
  }
}

\examples{
  data <- supportR::create_data()
  mod <- glm(female_ceo ~ board_size, data = data, family = "binomial")
  coefs <- coef(mod)
  logit_to_prob(logit = coefs[["board_size"]])
}
