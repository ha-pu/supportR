# .hlpr_get_col ----
.hlpr_get_col <- function(input, col) {
  out <- input[input$col == col,]
  out <- tidyr::pivot_wider(out, names_from = mod, values_from = val)
  out <- out[,-2]
  return(out)
}
