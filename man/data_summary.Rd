\name{data_summary}

\alias{data_summary}
\alias{data_summary.data.frame}
\alias{data_summary.grouped_df}
\alias{data_summary.numeric}
\alias{data_summary.logical}
\alias{data_summary.character}
\alias{data_summary.factor}

\title{Data summary}

\description{
  Generate automatic data summaries for different types of data
}

\usage{
  data_summary(input, ...)

  \method{data_summary}{data.frame}(input, at, show = TRUE)

  \method{data_summary}{grouped_df}(input, at, show = TRUE)

  \method{data_summary}{numeric}(input, show = TRUE)

  \method{data_summary}{logical}(input, show = TRUE)

  \method{data_summary}{character}(input, show = TRUE)

  \method{data_summary}{factor}(input, show = TRUE)
}

\arguments{
  \item{input}{Data that should be summarized, can be of class \code{"data.frame"}, \code{"grouped_df"}, \code{"numeric"}, \code{"logical"}, \code{"character"}, \code{"factor"}}
  \item{at}{\code{"character"} or \code{"integer"} vector that identifies the variables that should be summarized (only for \code{"data.frame"} and \code{"grouped_df"} methods)}
  \item{show}{Indicator whether output should be returned or passed to \code{"print"}}
}

\details{
  The function \code{data_summary} provides 11 columns of output for each variable selected in \code{at}:
  \itemize{
    \item{Variable}{variable name}
    \item{Cluster}{cluster for grouping}
    \item{Type}{variable type}
    \item{n}{number of non-missing observations}
    \item{SD}{standard deviation}
    \item{Min.}{minimum}
    \item{1st Qu.}{.25 percentile}
    \item{Mean}{mean}
    \item{Median}{median, .50 percentile}
    \item{3rd Qu.}{.75 percentile}
    \item{Max.}{maximum}
    \item{Groups}{number of distinct groups represented in the variable}
  }
  
  The exact output for the function depends on the data provided as \code{input}:
  \itemize{
    \item{\code{data.frame}}{The summary is provided for each variables selected in \code{at} in a separate row, the summary statistics depend on the variable type}
    \item{\code{grouped_df}}{As for \code{data.frame} but the summary is provided for each group separately, the respective group is shown in the variable \code{Cluster}}
    \item{\code{numeric}}{All output columns except "Groups"}
    \item{\code{logical}}{All output columns}
    \item{\code{character}}{Only output columns Variable, Type, n, Groups}
    \item{\code{factor}}{As for \code{character}}
  }
}

\value{
  Summary statistics for the data provided as \code{"input"} and the variables selected in \code{at}. When the option \code{show} is set to \code{TRUE} the output is printed to the console. If the package \code{knitr} is available, the output is printed using \code{knitr::kable}. When \code{show} is set to \code{FALSE} the output is returned as \code{data.frame} or \code{tibble} (when package \code{tibble} is available).
}

\examples{
  data <- supportR::create_data(n = 50)
  data$country <- as.factor(data$country)
  data_summary(data, at = c("firm_value", "female_ceo", "industry", "country"))
  data_summary(data$firm_value, show = FALSE)
  data_summary(dplyr::group_by(data, female_ceo), at = c("firm_value", "country"))
}
