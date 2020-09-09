#' @title Data summary
#'
#' @aliases
#' data_summary
#' data_summary.data.frame
#' data_summary.grouped_df
#' data_summary.numeric
#' data_summary.logical
#' data_summary.character
#' data_summary.factor
#'
#' @description
#' Generate automatic data summaries for different types of data.
#'
#' @details
#' The function \code{data_summary} provides 11 columns of output for each
#' variable selected in \code{at}:
#' \itemize{
#'   \item Variable - variable name
#'   \item Cluster - cluster for grouping
#'   \item Type - variable type
#'   \item n - number of non-missing observations
#'   \item SD - standard deviation
#'   \item Min. - minimum
#'   \item 1st Qu. - .25 percentile
#'   \item Mean - mean
#'   \item Median - median, .50 percentile
#'   \item 3rd Qu. - .75 percentile
#'   \item Max. - maximum
#'   \item Groups - number of distinct groups represented in the variable
#' }
#' The exact output for the function depends on the data provided as
#' \code{input}:
#'   \itemize{
#'     \item \code{data.frame} - The summary is provided for each variables
#'     selected in \code{at} in a separate row, the summary statistics
#'     depend on the variable type
#'     \item \code{grouped_df} - As for \code{data.frame} but the summary
#'     is provided for each group separately, the respective group is
#'     shown in the variable \code{Cluster}
#'     \item \code{numeric} - All output columns except "Groups"
#'     \item \code{logical} - All output columns
#'     \item \code{character} - Only output columns Variable, Type, n, Groups
#'     \item \code{factor} - As for \code{character}
#'   }
#'
#' @param input Data that should be summarized, can be of class
#' \code{"data.frame"}, \code{"grouped_df"}, \code{"numeric"},
#' \code{"logical"}, \code{"character"}, \code{"factor"}.
#' @param at \code{"character"} or \code{"integer"} vector that identifies
#' the variables that should be summarized (only for \code{"data.frame"}
#' and \code{"grouped_df"} methods).
#' @param show Indicator whether output should be returned or passed to \code{"print"}.
#'
#' @return Summary statistics for the data provided as \code{"input"} and
#' the variables selected in \code{at}. When the option \code{show} is set
#' to \code{TRUE} the output is printed to the console. If the package
#' \code{knitr} is available, the output is printed using \code{knitr::kable}.
#' When \code{show} is set to \code{FALSE} the output is returned as \code{data.frame}
#' or \code{tibble} (when package \code{tibble} is available).
#'
#'
#' @examples
#' data <- create_data(n = 50)
#' data_summary(data, at = c("ebit", "female_ceo", "sic", "country"))
#' data_summary(data$ebit, show = FALSE)
#' data_summary(dplyr::group_by(data, female_ceo), at = c("ebit", "country"))
#' @rdname data_summary
#' @export
#' @importFrom dplyr bind_rows
#' @importFrom dplyr ungroup
#' @importFrom tidyr gather
#' @importFrom tidyr spread

data_summary <- function(input, ...) UseMethod("data_summary", input)

#' @rdname data_summary
#' @method data_summary data.frame
#' @export

data_summary.data.frame <- function(input, at, show = TRUE, ...) {
  if (is.numeric(at)) {
    if (is.double(at)) {
      message("'at' of class 'double' is coerced to class 'integer'!")
      at <- as.integer(at)
    }
    at <- names(input)[at]
  }
  if (is.character(at)) {
    input <- as.data.frame(input)
    out <- lapply(at, function(x) .hlpr_get_summary(input[, x]))
    out <- cbind.data.frame(out)
    colnames(out) <- at
    out$Statistic <- stat

    out <- gather(out, -Statistic, key = "Variable", value = "value")
    out <- spread(out, key = Statistic, value = value)

    out <- out[c("Variable", stat)]
    out[, stat[-1]] <- sapply(out[, stat[-1]], as.numeric)

    if (show) {
      .hlpr_print_summary(input = out)
    } else {
      return(out)
    }
  } else {
    stop("'at' must be class 'character' or 'numeric'!")
  }
}

#' @rdname data_summary
#' @method data_summary grouped_df
#' @export

data_summary.grouped_df <- function(input, at, show = TRUE, ...) {
  if (ncol(attr(input, "groups")) > 2) {
    warning("'data_summary' uses only the first group for clustering!")
  }
  out <- lapply(attr(input, "groups")$.rows, function(x) {
    out <- data_summary(input = ungroup(input[x, ]), at = at, show = FALSE)
    return(out)
  })
  out <- bind_rows(out)
  out$Cluster <- paste(names(attr(input, "groups"))[[1]], rep(as.data.frame(attr(input, "groups"))[, 1], each = length(at)), sep = ": ")
  out <- out[, c("Variable", "Cluster", stat)]
  out <- out[order(out$Variable, out$Cluster), ]

  if (show) {
    .hlpr_print_summary(input = out)
  } else {
    if (requireNamespace("tibble", quietly = TRUE)) {
      out <- tibble::as_tibble(out)
    }
    return(out)
  }
}

#' @rdname data_summary
#' @method data_summary numeric
#' @export

data_summary.numeric <- function(input, show = TRUE, ...) {
  input <- as.data.frame(input)
  out <- data_summary(input = input, at = "input", show = FALSE)

  if (show) {
    .hlpr_print_summary(input = out)
  } else {
    return(out)
  }
}

#' @rdname data_summary
#' @method data_summary logical
#' @export

data_summary.logical <- data_summary.numeric

#' @rdname data_summary
#' @method data_summary character
#' @export

data_summary.character <- data_summary.numeric

#' @rdname data_summary
#' @method data_summary factor
#' @export

data_summary.factor <- data_summary.numeric

stat <- c("Type", "n", "SD", "Min.", "1st Qu.", "Mean", "Median", "3rd Qu.", "Max.", "Groups")
