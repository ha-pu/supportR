\name{standardize_data}

\alias{standardize_data}
\alias{standardize_data.data.frame}

\title{Standardize data}

\description{
  Standarize (center and scale) all numeric variables in a data.frame.
}

\usage{
  standardize_data(data, center = TRUE, scale = TRUE)
}

\arguments{
  \item{data}{\code{data.frame} that contains the data for standardization}
  \item{center}{Value \code{TRUE} or \code{FALSE} that is passed to function \code{scale}}
  \item{scale}{Value \code{TRUE} or \code{FALSE} that is passed to function \code{scale}}
}

\details{
  The function applies the \code{base} function \code{scale} to all numeric variables in the input \code{data.frame}. When the input options \code{base} and \code{scale} do not receive logical values as input, the option defaults to \code{TRUE}.
}

\value{
  \code{data.frame} or \code{tibble} (when package \code{tibble} is available) object containing all varibales from the input \code{data.frame} where all numeric variables are scaled according to \code{base} and \code{scale}.
}

\seealso{
  \code{\link[base]{scale}}
}

\examples{
  data <- supportR::create_data()
  standardize_data(data = data, center = TRUE, scale = TRUE)
  standardize_data(data = data, center = TRUE, scale = FALSE)
}
