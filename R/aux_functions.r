# .paste1 helper function ----
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

# create data function ----
create_data <- function(variables) {
  data_size <- 1000
  weights = c(2, -1.5, -0.8, 1.9, 0.5, 0, 0, 0, 0)
  
  data_value <- data.frame(
    profit = rnorm(data_size),
    cogs = rnorm(data_size),
    fixed_cost = rnorm(data_size),
    rnd = rnorm(data_size),
    assets = rnorm(data_size),
    competition = rnorm(data_size),
    quality = rnorm(data_size),
    board_size = rnorm(data_size),
    ceo_age = rnorm(data_size)
  )
  data_value$firm_value <- rowSums(data_value * weights)
  
  data_value$female_ceo = as.logical(rbinom(data_size, 1, 0.5))
  data_value$firm_value <- data_value$firm_value + data_value$female_ceo * 0.1
  
  data_value$industry = sample(c("A", "B", "C", "D"), data_size, replace = TRUE)
  data_value$firm_value[data_value$industry == "A"] <- data_value$firm_value[data_value$industry == "A"] + 0.2
  data_value$firm_value[data_value$industry == "B"] <- data_value$firm_value[data_value$industry == "B"] + 0.1
  data_value$firm_value[data_value$industry == "C"] <- data_value$firm_value[data_value$industry == "C"] - 0.1
  data_value$firm_value[data_value$industry == "D"] <- data_value$firm_value[data_value$industry == "D"] - 0.2
  
  data_value$country = sample(c("A", "B", "C", "D"), data_size, replace = TRUE)
  data_value$firm_value[data_value$country == "A"] <- data_value$firm_value[data_value$country == "A"] + 0.2
  data_value$firm_value[data_value$country == "B"] <- data_value$firm_value[data_value$country == "B"] + 0.1
  data_value$firm_value[data_value$country == "C"] <- data_value$firm_value[data_value$country == "C"] - 0.1
  data_value$firm_value[data_value$country == "D"] <- data_value$firm_value[data_value$country == "D"] - 0.2
  
  data_value$firm_value <- data_value$firm_value + rnorm(data_size)
  
  if (requireNamespace("tibble", quietly = TRUE)) {
	data_value <- tibble::as_tibble(data_value)
  }
  
  return(data_value)
}
