# .hlpr_get_matrix ----
.hlpr_get_matrix <- function (data) {
  if ((all(data$Parameter1 %in% data$Parameter2) && all(data$Parameter2 %in% data$Parameter1))) {
    vars <- as.character(unique(c(data$Parameter1, data$Parameter2)))
    dim <- length(vars)
    m <- matrix(nrow = dim, ncol = dim, dimnames = list(vars, vars))
  }
  else {
    m <- matrix(nrow = length(unique(data$Parameter1)), ncol = length(unique(data$Parameter2)), dimnames = list(unique(data$Parameter1), unique(data$Parameter2)))
  }
  m[] <- 1

  return(m)
}
