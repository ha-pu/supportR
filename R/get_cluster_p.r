get_cluster_p <- function(model, var_cluster, use_bootstrap = TRUE, print = TRUE, stargazer = FALSE) {
  
  # to do: control class(var_cluster) %in% c(vector, matrix, data.frame, tibble?)
  
  # run checks ----
  if (nrow(model$model) != length(var_cluster)) stop("nrow(model$model) must equal length(var_cluster)!")
  
  # run for "lm" %in% class(model) ----
  if ("lm" %in% class(model)) {

    # compute variance covariance matrix ----
    mat <- vcov(model)
    if (!use_bootstrap) {
      vcov_mat <- multiwayvcov::cluster.vcov(model, var_cluster)
    } else if (use_bootstrap) {
      vcov_mat <- multiwayvcov::cluster.boot(model, var_cluster, R = 1000, boot_type = "wild", wild_type = function() sample(c(-1, 1), 1))
    }
    colnames(vcov_mat) <- colnames(mat)
    row.names(vcov_mat) <- row.names(mat)

    # compute se and p-values ----
    coef <- model$coefficients
    se <- lmtest::coeftest(model, vcov_mat)[,2]
    t_value <- coef / se
    p_value <- 2 * pt(-abs(t_value), df = model$df.residual)

    # export data ----
    out <- data.frame(Coefficients = names(coef),
                      Estimate = coef,
                      Std_Error = se,
                      t_value,
                      p_value,
                      row.names = NULL)

	if (requireNamespace("tibble", quietly = TRUE)) {
		out <- tibble::as_tibble(out)
	}
	
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
      if (stargazer) {
        warning("'print == TRUE' overrules 'stargazer == TRUE'")
      }
      print(out)
    } else {
      if (stargazer) {
        out <- list(model, se)
      }
      return(out)
    }
  } else {
	warning("Function currently only supports class lm!")
  }
  
  # to do: "glm" %in% class(model)
}
