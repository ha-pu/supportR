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
