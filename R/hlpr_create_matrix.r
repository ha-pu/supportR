#' @title create_matrix
#'
#' @keywords internal

.hlpr_create_matrix <- function(frame, object, column = "r") {
  if ("Group" %in% names(object)) {
    out <- data.frame()
    for (g in unique(object$Group)) {
      data <- object[object$Group == g, ]
      m <- .hlpr_fill_matrix(frame, data, column = column)
      m$Group <- g
      out <- rbind(out, m)
    }
    out <- out[c("Group", names(out)[names(out) != "Group"])]
  } else {
    out <- .hlpr_fill_matrix(frame, object, column = column)
  }

  return(out)
}
