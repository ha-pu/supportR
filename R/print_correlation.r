# print_correlation generic function ----
print_correlation <- function(data, ...) UseMethod("print_correlation", data)

# print_correlation data.frame method ----
print_correlation.data.frame <- function(data, show = TRUE, html = FALSE, ...) {

  # run correlation ----
  cor <- correlation::correlation(data)

  # do summary ----
  if (show | html) {
    out <- .hlpr_summary_cor(object = cor)
  }

  # return output ----
  if(html) {
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
