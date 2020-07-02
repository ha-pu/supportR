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
  \itemize{
    \item{profit: double - simulated as \code{rnorm(n)}}
    \item{cogs: double - simulated as \code{rnorm(n)}}
    \item{fixed_cost: double - simulated as \code{rnorm(n)}}
    \item{rnd: double - simulated as \code{rnorm(n)}}
    \item{assets: double - simulated as \code{rnorm(n)}}
    \item{competition: double - simulated as \code{rnorm(n)}}
    \item{quality: double - simulated as \code{rnorm(n)}}
    \item{board_size: double - simulated as \code{rnorm(n)}}
    \item{ceo_age: double - simulated as \code{rnorm(n)}}
    \item{female_ceo: logical - simulated as \code{as.logical(rbinom(n, 1, 0.5))}}
    \item{industry: character - simulated as \code{sample(c("A", "B", "C", "D"), n, replace = TRUE)}}
    \item{country: character - simulated as \code{sample(c("A", "B", "C", "D"), n, replace = TRUE)}}
    \item{firm_value: double - simulated as the result of the other variables, using random weights (\code{runif()}) and a random error term (\code{rnorm()})}
  }
}

\value{
  \code{data.frame} or \code{tibble} (when package \code{tibble} is available) with \code{n} rows and 13 columns.
}

\examples{
  create_data()
  create_data(n = 50)
}
