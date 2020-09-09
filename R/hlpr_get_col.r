#' @title get_col
#'
#' @keywords internal
#' @importFrom tidyr pivot_wider

.hlpr_get_col <- function(input, col) {
  out <- input[input$col == col, ]
  out <- pivot_wider(out, names_from = mod, values_from = val)
  out <- out[, -2]
  return(out)
}
