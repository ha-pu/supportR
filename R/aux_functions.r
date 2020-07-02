# .paste1 [helper] ----
.paste1 <- function(element1, element2, sep, collapse) {
  out <- c()
  for (i in element2) {
    for (j in element1) {
      out <- c(out, paste(i, j, sep = sep))
    }
  }
  out <- paste(out, collapse = collapse)
  return(out)
}

# create_data ----
create_data <- function(n = 1000) {
  weights = runif(9, min = -2, max = 2)
  
  data_value <- data.frame(
    profit = rnorm(n),
    cogs = rnorm(n),
    fixed_cost = rnorm(n),
    rnd = rnorm(n),
    assets = rnorm(n),
    competition = rnorm(n),
    quality = rnorm(n),
    board_size = rnorm(n),
    ceo_age = rnorm(n)
  )
  data_value$firm_value <- rowSums(data_value * weights)
  
  data_value$female_ceo = as.logical(rbinom(n, 1, 0.5))
  data_value$firm_value <- data_value$firm_value + data_value$female_ceo * runif(1, -0.1, 0.1)
  
  data_value$industry = sample(c("A", "B", "C", "D"), n, replace = TRUE)
  data_value$firm_value[data_value$industry == "A"] <- data_value$firm_value[data_value$industry == "A"] + runif(1, 0, 0.2)
  data_value$firm_value[data_value$industry == "B"] <- data_value$firm_value[data_value$industry == "B"] + runif(1, -0.1, 0.1)
  data_value$firm_value[data_value$industry == "C"] <- data_value$firm_value[data_value$industry == "C"] - runif(1, -0.1, 0.1)
  data_value$firm_value[data_value$industry == "D"] <- data_value$firm_value[data_value$industry == "D"] - runif(1, 0, 0.2)
  
  data_value$country = sample(c("A", "B", "C", "D"), n, replace = TRUE)
  data_value$firm_value[data_value$country == "A"] <- data_value$firm_value[data_value$country == "A"] + runif(1, 0, 0.2)
  data_value$firm_value[data_value$country == "B"] <- data_value$firm_value[data_value$country == "B"] + runif(1, -0.1, 0.1)
  data_value$firm_value[data_value$country == "C"] <- data_value$firm_value[data_value$country == "C"] - runif(1, -0.1, 0.1)
  data_value$firm_value[data_value$country == "D"] <- data_value$firm_value[data_value$country == "D"] - runif(1, 0, 0.2)
  
  data_value$firm_value <- data_value$firm_value + rnorm(n)
  
  if (requireNamespace("tibble", quietly = TRUE)) {
    data_value <- tibble::as_tibble(data_value)
  }
  
  return(data_value)
}

# clean_names ----
clean_names <- function(data) {
	
  # run clean_names ----
  if (requireNamespace("janitor", quietly = TRUE)) {
    out <- janitor::clean_names(data)
	return(out)
  } else {
    warning("'clean_names' requires the 'jantitor' package!")
  }
}

# standardize_data generic function ----
standardize_data <- function(data, center = TRUE, scale = TRUE, ...) UseMethod("standardize_data", data)

# standardize_data data.frame method ----
standardize_data.data.frame <- function(data, center = TRUE, scale = TRUE, ...) {
	
  # run checks ----
  if (!is.logical(center)) {
    center <- TRUE
    warning("center must be a logical value! Default for center is TRUE.")
  }
  if (!is.logical(scale)) {
	scale <- TRUE
	warning("scale must be a logical value! Default for scale is TRUE.")
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

# logit_to_prob ----
logit_to_prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}
