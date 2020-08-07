# clean_names ----
clean_names <- function(data) {
	
  # run clean_names ----
  if (requireNamespace("janitor", quietly = TRUE)) {
    out <- janitor::clean_names(data)
	return(out)
  } else {
    stop("'clean_names' requires the 'jantitor' package!")
  }
}
