\name{create_data}

\alias{create_data}

\title{Create data}

\description{
  Generate simulated dataset that can be used for testing and reproducible examples.
}

\usage{
  create_data(n = 1000)
}

\arguments{
  \item{n}{Number of observations to be simulated}
}

\details{
  The function \code{create_data} generates a \code{data.frame} with \code{n} rows (defaults to 1,000) and the following 13 columns:
  \item{\code{profit}}{\code{double} - simulated as \code{rnorm(n)}}
  \item{\code{cogs}}{\code{double} - simulated as \code{rnorm(n)}}
  \item{\code{fixed_cost}}{\code{double} - simulated as \code{rnorm(n)}}
  \item{\code{rnd}}{\code{double} - simulated as \code{rnorm(n)}}
  \item{\code{assets}}{\code{double} - simulated as \code{rnorm(n)}}
  \item{\code{competition}}{\code{double} - simulated as \code{rnorm(n)}}
  \item{\code{quality}}{\code{double} - simulated as \code{rnorm(n)}}
  \item{\code{board_size}}{\code{double} - simulated as \code{rnorm(n)}}
  \item{\code{ceo_age}}{\code{double} - simulated as \code{rnorm(n)}}
  \item{\code{female_ceo}}{\code{logical} - simulated as \code{as.logical(rbinom(n, 1, 0.5))}}
  \item{\code{industry}}{\code{character} - simulated as \code{sample(c("A", "B", "C", "D"), n, replace = TRUE)}}
  \item{\code{country}}{\code{character} - simulated as \code{sample(c("A", "B", "C", "D"), n, replace = TRUE)}}
  \item{\code{firm_value}}{\code{double} - simulated as the result of the other variables, using random weights (\code{runif()}) and a random error term (\code{rnorm()})}
}

\value{
  \code{data.frame} or \code{tibble} (when package \code{tibble} is available) with \code{n} rows and 13 columns.
}

\examples{
  create_data()
  create_data(n = 50)
}
