# .hlpr_paste ----
.hlpr_paste <- function(element1, element2, sep, collapse) {
  out <- c()
  for (i in element2) {
    for (j in element1) {
      out <- c(out, paste(i, j, sep = sep))
    }
  }
  out <- paste(out, collapse = collapse)
  return(out)
}
