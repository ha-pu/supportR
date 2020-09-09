#' @title get_uniques
#'
#' @keywords internal

.hlpr_get_uniques <- function(input) {
  out <- length(unique(input))
  return(out)
}
