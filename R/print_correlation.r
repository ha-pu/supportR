#' @title Print correlation matrix
#'
#' @aliases
#' print_correlation
#' print_correlation.data.frame
#'
#' @description
#' The function uses the \code{correlation} function from the \code{correlation}
#' package to prepare correlation matrices. The options \code{show} and \code{html}
#' allow to adapt the output form.
#'
#' @param data \code{data.frame} or \code{tibble} containing the data to compute
#' the correlation
#' @param show value \code{TRUE} or \code{FALSE} if output should be printed to
#' console as plain text
#' @param html value \code{TRUE} or \code{FALSE} if output should be printed to
#' console as html code
#'
#' @return
#' \itemize{
#'   \item \code{show == TRUE & html == FALSE} The correlation matrix prints to the
#' console as plain text
#'   \item \code{show == TRUE & html == TRUE} The correlation matrix prints to the
#' console as html code
#'   \item \code{show == FALSE & html == FALSE} The functions returns a ctable of
#' correlation coefficients and p-values - see \code{correlation::correlation} for further details
#' }
#'
#' @seealso \code{\link[correlation]{correlation}}
#'
#' @examples
#' data <- create_data()
#' print_correlation(data = data, show = TRUE, html = FALSE)
#' print_correlation(data = data, show = TRUE, html = TRUE)
#' print_correlation(data = data, show = FALSE, html = TRUE)
#' # html = TRUE overrules show = FALSE
#' print_correlation(data = data, show = FALSE, html = FALSE)
#' @rdname print_correlation
#' @export
#' @importFrom correlation correlation

print_correlation <- function(data, ...) UseMethod("print_correlation", data)

#' @rdname print_correlation
#' @method print_correlation data.frame
#' @export

print_correlation.data.frame <- function(data, show = TRUE, html = FALSE, ...) {

  # run correlation ----
  cor <- correlation(data)

  # do summary ----
  if (show | html) {
    out <- .hlpr_summary_cor(object = cor)
  }

  # return output ----
  if (html) {
    if (requireNamespace("stargazer", quietly = TRUE)) {
      if (!show) message("html == TRUE overrules show == FALSE!")
      out <- .hlpr_print_cor(object = out, html = TRUE)
      return(out)
    } else {
      stop("html == TRUE requires 'stargazer' package!")
    }
  } else if (show) {
    .hlpr_print_cor(object = out)
  } else {
    return(cor)
  }
}
