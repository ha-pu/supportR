#' @title get_summary
#'
#' @aliases
#' .hlpr_get_summary
#' .hlpr_get_summary.numeric
#' .hlpr_get_summary.logical
#' .hlpr_get_summary.character
#' .hlpr_get_summary.factor
#'
#' @keywords internal

.hlpr_get_summary <- function(input, ...) UseMethod(".hlpr_get_summary", input)

#' @method .hlpr_get_summary numeric
#' @keywords internal

.hlpr_get_summary.numeric <- function(input, ...) {
  input <- input[!is.na(input)]
  out <- c(
    .hlpr_get_stats(input),
    NA
  )
  return(out)
}

#' @method .hlpr_get_summary logical
#' @keywords internal

.hlpr_get_summary.logical <- function(input, ...) {
  input <- input[!is.na(input)]
  out <- c(
    .hlpr_get_stats(input),
    .hlpr_get_uniques(input)
  )
  return(out)
}

#' @method .hlpr_get_summary character
#' @keywords internal

.hlpr_get_summary.character <- function(input, ...) {
  input <- input[!is.na(input)]
  out <- c(
    class(input),
    length(input),
    NA,
    NA,
    NA,
    NA,
    NA,
    NA,
    NA,
    .hlpr_get_uniques(input)
  )
  return(out)
}


#' @method .hlpr_get_summary factor
#' @keywords internal

.hlpr_get_summary.factor <- .hlpr_get_summary.character
