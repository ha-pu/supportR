# .hlpr_print_summary ----
.hlpr_print_summary <- function(input) {
  if (requireNamespace("knitr", quietly = TRUE)) {
    knitr::kable(input, digits = 3)
  } else {
    input[,stat] <- round(input[,stat], digits = 3)
    print(input)
  }
}
