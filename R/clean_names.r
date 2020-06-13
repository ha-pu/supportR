clean_names <- function(data) {
	
	# to do: check data input
	
	# run clean_names ----
	out <- janitor::clean_names(data)
	
	# return output ----
	return(out)
}