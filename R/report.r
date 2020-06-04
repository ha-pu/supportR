# report generic function ----
report <- function(model_list, ...) UseMethod("report", model_list)

# method for class "modular_glm" ----
report.modular_glm <- function(model_list, cv, cluster, cluster_boot = FALSE, data, show = TRUE, show_cv = FALSE, ...) {
  class(model_list) <- "list"
  
  if (!is.null(cluster)) {
    cluster <- data[,cluster]
    if ("tbl" %in% class(cluster)) {
      cluster <- cluster[[1]]
    }
  }
  
  out <- purrr::map(seq(length(model_list)), ~{
    out <- get_p(model_list[[.x]], var_cluster = cluster, cluster_boot = cluster_boot, show = FALSE)
    out$mod <- paste0("mod_", .x)
    return(out)
  })
  out <- dplyr::bind_rows(out)
  out <- tidyr::pivot_longer(out, -c(Coefficients, mod), names_to = "col", values_to = "val")
  
  out_estimate <- out[out$col == "Estimate",]
  out_estimate <- tidyr::pivot_wider(out_estimate, names_from = mod, values_from = val)
  out_estimate <- out_estimate[,-2]
  
  out_se <- out[out$col == "Std_Error",]
  out_se <- tidyr::pivot_wider(out_se, names_from = mod, values_from = val)
  out_se <- out_se[,-2]
  
  out_pvalue <- out[out$col == "p_value",]
  out_pvalue <- tidyr::pivot_wider(out_pvalue, names_from = mod, values_from = val)
  out_pvalue <- out_pvalue[,-2]
  
  if (!show_cv) {
    out_estimate <- out_estimate[!(out_estimate$Coefficients %in% c("(Intercept)", cv)), -2]
    out_se <- out_se[!(out_se$Coefficients %in% c("(Intercept)", cv)), -2]
    out_pvalue <- out_pvalue[!(out_pvalue$Coefficients %in% c("(Intercept)", cv)), -2]
  }
  
  out <- list(out_estimate, out_se, out_pvalue)
  names(out) <- c("coefficients", "std.errors", "p.values")
  
  if (show) {
    print(purrr::map(out, ~ knitr::kable(.x, digits = 3)))
  } else {
    return(out)
  }
}

# method for class "modular_glmer" ----
report.modular_glmer <- function(model_list, cv, show = TRUE, show_cv = FALSE, ...) {
  class(model_list) <- "list"
  
  out <- purrr::map(seq(length(model_list)), ~{
    out <- get_p(model_list[[.x]], show = FALSE)
    out$mod <- paste0("mod_", .x)
    return(out)
  })
  out <- dplyr::bind_rows(out)
  out <- tidyr::pivot_longer(out, -c(Coefficients, mod), names_to = "col", values_to = "val")
  
  out_estimate <- out[out$col == "Estimate",]
  out_estimate <- tidyr::pivot_wider(out_estimate, names_from = mod, values_from = val)
  out_estimate <- out_estimate[,-2]
  
  out_se <- out[out$col == "Std_Error",]
  out_se <- tidyr::pivot_wider(out_se, names_from = mod, values_from = val)
  out_se <- out_se[,-2]
  
  out_pvalue <- out[out$col == "p_value",]
  out_pvalue <- tidyr::pivot_wider(out_pvalue, names_from = mod, values_from = val)
  out_pvalue <- out_pvalue[,-2]
  
  out <- list(out_estimate, out_se, out_pvalue)
  names(out) <- c("coefficients", "std.errors", "p.values")
  
  if (!show_cv) {
    out_estimate <- out_estimate[!(out_estimate$Coefficients %in% c("(Intercept)", cv)), -2]
    out_se <- out_se[!(out_se$Coefficients %in% c("(Intercept)", cv)), -2]
    out_pvalue <- out_pvalue[!(out_pvalue$Coefficients %in% c("(Intercept)", cv)), -2]
  }
  
  if (show) {
    print(purrr::map(out, ~ knitr::kable(.x, digits = 3)))
  } else {
    return(out)
  }
}
