#' @title get_stats
#'
#' @keywords internal
#' @importFrom stats median
#' @importFrom stats quantile
#' @importFrom stats sd

.hlpr_get_stats <- function(input) {
  out <- c(
    class(input),
    length(input),
    sd(input),
    min(input),
    quantile(input, 0.25),
    mean(input),
    median(input),
    quantile(input, 0.75),
    max(input)
  )
  return(out)
}
