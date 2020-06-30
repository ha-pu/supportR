\name{data_summary}
\alias{data_summary}
\alias{data_summary.data.frame}
\alias{data_summary.grouped_df}
\alias{data_summary.numeric}
\alias{data_summary.logical}
\alias{data_summary.character}
\alias{data_summary.factor}
\title{Data summary}
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
\value{
Summary statistics for the data provided as \code{"input"}
}
\description{
Generate automatic data summaries for different types of data
}
