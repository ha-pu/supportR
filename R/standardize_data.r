#' @title Standardize data
#'
#' @aliases
#' standardize_data
#' standardize_data.data.frame
#'
#' @description
#' Standarize (center and scale) all numeric variables in a data.frame.
#'
#' @details
#' The function applies the \code{base} function \code{scale} to all numeric variables
#' in the input \code{data.frame}. When the input options \code{base} and \code{scale}
#' do not receive logical values as input, the option defaults to \code{TRUE}.
#'
#' @param data \code{data.frame} that contains the data for standardization
#' @param center Value \code{TRUE} or \code{FALSE} that is passed to function \code{scale}
#' @param scale Value \code{TRUE} or \code{FALSE} that is passed to function \code{scale}
#'
#' @return  \code{data.frame} or \code{tibble} (when package \code{tibble} is available)
#' object containing all varibales from the input \code{data.frame} where all numeric
#' variables are scaled according to \code{base} and \code{scale}.
#'
#' @seealso \code{\link[base]{scale}}
#'
#' @examples
#' data <- create_data()
#' standardize_data(data = data, center = TRUE, scale = TRUE)
#' standardize_data(data = data, center = TRUE, scale = FALSE)
#' @rdname standardize_data
#' @export

standardize_data <- function(data, center = TRUE, scale = TRUE, ...) UseMethod("standardize_data", data)

#' @rdname standardize_data
#' @method standardize_data data.frame
#' @export

standardize_data.data.frame <- function(data, center = TRUE, scale = TRUE, ...) {

  # run checks ----
  if (!is.logical(center)) {
    center <- TRUE
    warning("center must be a logical value! Use default value for center TRUE.")
  }
  if (!is.logical(scale)) {
    scale <- TRUE
    warning("scale must be a logical value! Use default value for scale TRUE.")
  }

  # run standardization ----
  numeric_vars <- unlist(lapply(data, is.numeric))
  scaled_data <- scale(data[, numeric_vars], center = center, scale = scale)
  out <- cbind.data.frame(scaled_data, data[, !numeric_vars])

  # return output ----
  if (requireNamespace("tibble", quietly = TRUE)) {
    out <- tibble::as_tibble(out)
  }

  return(out)
}
