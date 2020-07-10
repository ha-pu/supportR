# .hlpr_fill_matrix ----
.hlpr_fill_matrix <- function(frame, object, column = "r") {
  for (row in row.names(frame)) {
    for (col in colnames(frame)) {
      frame[row, col] <- object[(object$Parameter1 == row & object$Parameter2 == col) | (object$Parameter2 == row & object$Parameter1 == col), column][1]
    }
  }

  # Add Parameter column
  frame <- as.data.frame(frame)
  frame$Parameter <- row.names(frame)
  frame <- frame[c("Parameter", names(frame)[names(frame) != "Parameter"])]
  row.names(frame) <- NULL

  # Remove upper triangular
  frame[-1][lower.tri(frame[-1])] <- NA
  frame <- frame[c(1, ncol(frame):2)]

  return(frame)
}
