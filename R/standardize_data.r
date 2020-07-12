# standardize_data generic function ----
standardize_data <- function(data, center = TRUE, scale = TRUE, ...) UseMethod("standardize_data", data)

# standardize_data data.frame method ----
standardize_data.data.frame <- function(data, center = TRUE, scale = TRUE, ...) {
	
  # run checks ----
  if (!is.logical(center)) {
    center <- TRUE
    warning("center must be a logical value! Use default value for center TRUE.")
  }
  if (!is.logical(scale)) {
	scale <- TRUE
	warning("scale must be a logical value! Use default value for scale TRUE.")
  }
	
  # run standardization ----
  numeric_vars <- unlist(lapply(data, is.numeric))
  scaled_data <- scale(data[,numeric_vars], center = center, scale = scale)
  out <- cbind.data.frame(scaled_data, data[,!numeric_vars])

  # return output ----
  if (requireNamespace("tibble", quietly = TRUE)) {
	out <- tibble::as_tibble(out)
  }

  return(out)	
}
