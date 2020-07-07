# .hlpr_summary_cor ----
.hlpr_summary_cor <- function(object, stars = TRUE) {

  frame <- .hlpr_get_matrix(object)

  target_col <- names(object)[names(object) %in% c("r", "rho", "tau", "Median")][1]
  if (is.na(target_col)) {
    target_col <- names(object)[!names(object) %in% c("Parameter1", "Parameter2")][1]
  }

  out <- .hlpr_create_matrix(frame, object, column = target_col)

  # Fill attributes
  for (i in names(object)[!names(object) %in% c("Group", "Parameter1", "Parameter2", target_col)]) {
    attri <- .hlpr_create_matrix(frame, object, column = i)
    attr(out, i) <- attri
  }

  # Transfer attributes
  attributes(out) <- c(attributes(out), attributes(object)[!names(attributes(object)) %in% c("names", "row.names", "class", names(attributes(out)))])
  attr(out, "stars") <- stars
  attr(out, "coefficient_name") <- target_col

  return(out)
}
