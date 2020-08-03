\name{print_correlation}

\alias{print_correlation}
\alias{print_correlation.data.frame}

\title{Print correlation matrix}

\description{
  The function uses the \code{correlation} function from the \code{correlation} package to prepare correlation matrices. The options \code{show} and \code{html} allow to adapt the output form.
}

\usage{
  print_correlation(data, show, html)
}

\arguments{
  \item{data}{\code{data.frame} or \code{tibble} containing the data to compute the correlation}
  \item{show}{value \code{TRUE} or \code{FALSE} if output should be printed to console as plain text}
  \item{html}{value \code{TRUE} or \code{FALSE} if output should be printed to console as html code}
}

\value{
  \item{\code{show == TRUE & html == FALSE}}{The correlation matrix prints to the console as plain text}
  \item{\code{show == TRUE & html == TRUE}}{The correlation matrix prints to the console as html code}
  \item{\code{show == FALSE & html == FALSE}}{The functions returns a ctable of correlation coefficients and p-values - see \code{correlation::correlation} for further details}
}

\seealso{
  \code{\link[correlation]{correlation}}
}

\examples{
  data <- supportR::create_data()
  print_correlation(data = data, show = TRUE, html = FALSE)
  print_correlation(data = data, show = TRUE, html = TRUE)
  print_correlation(data = data, show = FALSE, html = TRUE)
  # html = TRUE overrules show = FALSE
  print_correlation(data = data, show = FALSE, html = FALSE)
}
