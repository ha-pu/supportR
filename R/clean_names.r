#' @title
#' Clean names of an object.
#'
#' @description
#' The function imports the \code{clean_names} function from the \code{janitor} package.
#'
#' @section Warning:
#' The function requires that the \code{janitor} is installed!
#'
#' @seealso \code{\link[janitor]{clean_names}}
#'
#' @param data Object, usually \code{data.frame}, with column names to clean.
#' @return The input object \code{data} with clean column names.
#' @examples
#' names(data) <- gsub("_", ".", names(data))
#' clean_names(data)
#' @importFrom janitor clean_names

# clean_names ----
clean_names <- function(data) {

  # run clean_names ----
  if (requireNamespace("janitor", quietly = TRUE)) {
    out <- janitor::clean_names(data)
	return(out)
  } else {
    stop("'clean_names' requires the 'jantitor' package!")
  }
}
