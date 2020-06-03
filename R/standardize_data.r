standardize_data <- function(data, center = TRUE, scale = TRUE) {
	
	# run checks ----
	if (!is.logical(center)) {
		center <- TRUE
		warning("center must be a logical value! Default for center is TRUE.")
	}
	if (!is.logical(scale)) {
		scale <- TRUE
		warning("scale must be a logical value! Default for scale is TRUE.")
	}
	
	# run for "data.frame" %in% class(data) | "tbl_df" %in% class(data) ----
	if ("data.frame" %in% class(data) | "tbl_df" %in% class(data)) {
		# run standardization ----
		numeric_vars <- unlist(lapply(data, is.numeric))
		scaled_data <- scale(data[,numeric_vars], center = center, scale = scale)
		out <- cbind.data.frame(scaled_data, data[,!numeric_vars])

		# return output ----
		if (requireNamespace("tibble", quietly = TRUE)) {
			out <- tibble::as_tibble(out)
		}

		return(out)
	} else {
		warning("class(data) must be 'data.frame' or 'tbl_df'!")
	}	
}