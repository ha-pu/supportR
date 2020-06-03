get_lmer_p <- function(model, print = TRUE) {
  
  # run for "lmerMod" %in% class(model) ----
  if ("lmerMod" %in% class(model)) {
    
    # get data ----
    name <- attributes(model@pp$X)$dimnames[[2]]
    coef <- model@beta
    se <- sqrt(diag(vcov(model)))
    t_value <- coef / se
    p_value <- 2 * pt(-abs(t_value), df = nrow(model@frame) - 1)
    out <- data.frame(Coefficients = name,
                      Estimate = coef,
                      Std_Error = se,
                      t_value,
                      p_value,
                      row.names = NULL)
	if (requireNamespace("tibble", quietly = TRUE)) {
		out <- tibble::as_tibble(out)
	}
	
    # export data ----
    if (print) {
      out$Estimate <- round(out$Estimate, digits = 3)
      out$Std_Error <- round(out$Std_Error, digits = 3)
      out$t_value <- round(out$t_value, digits = 3)
      out$p_value <- round(out$p_value, digits = 3)
      out$` ` <- ""
      out$` `[out$p_value <= 0.1] <- "."
      out$` `[out$p_value <= 0.05] <- "*"
      out$` `[out$p_value <= 0.01] <- "**"
      out$` `[out$p_value <= 0.001] <- "***"
      print(out)
    } else {
      return(out) 
    }
  } else {
	warning("Function currently only supports class lmerMod!")
  }
  
  # to do: "glmerMod" %in% class(model)
}
