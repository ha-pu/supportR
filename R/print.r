#' @title Print
#'
#' @rdname print
#' @method print model_summary
#' @export

print.model_summary <- function(x, ...) {

  # prepate data ----
  x$sig <- ""
  x$sig[x$p_value <= 0.1] <- "."
  x$sig[x$p_value <= 0.05] <- "*"
  x$sig[x$p_value <= 0.01] <- "**"
  x$sig[x$p_value <= 0.001] <- "***"

  if (requireNamespace("knitr", quietly = TRUE)) {
    knitr::kable(x, digits = 3)
  } else {
    x$Estimate <- round(x$Estimate, digits = 3)
    x$Std_Error <- round(x$Std_Error, digits = 3)
    x$t_value <- round(x$t_value, digits = 3)
    x$p_value <- round(x$p_value, digits = 3)
    print(x)
  }
}

#' @rdname print
#' @method print model_randfe
#' @export

print.model_randfe <- function(x, ...) {
  if (requireNamespace("knitr", quietly = TRUE)) {
    knitr::kable(x, digits = 3)
  } else {
    x$Estimate <- round(x$Estimate, digits = 3)
    print(x)
  }
}
