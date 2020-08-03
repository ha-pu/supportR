\name{clean_names}

\alias{clean_names}

\title{Clean names of an object}

\description{
  The function imports the \code{clean_names} function from the \code{janitor} package.
}

\usage{
  clean_names(data)
}

\arguments{
  \item{data}{Object, usually \code{data.frame}, with names to clean}
}

\details{
  The function imports the \code{clean_names} function from the \code{janitor} package. See the documentaiton in the \code{janitor} package for further details. The function requires that the \code{janitor} is installed!
}

\value{
  \code{data.frame} with clean names
}

\seealso{
  \code{\link[janitor]{clean_names}}
}

\examples{
  data <- supportR::create_data()
  names(data) <- gsub("_", ".", names(data))
  clean_names(data)
}
