#' @title modular_report
#'
#' @keywords internal
#' @importFrom dplyr bind_rows
#' @importFrom tidyr pivot_longer

.hlpr_modular_report <- function(model_list, cv, show_cv, robust_type, var_cluster) {
  out <- lapply(seq(length(model_list)), function(x) {
    out <- model_summary(input = model_list[[x]], type = robust_type, var_cluster = var_cluster, show = FALSE)
    out$mod <- paste0("mod_", x)
    return(out)
  })

  out <- bind_rows(out)
  out <- pivot_longer(out, -c(Variables, mod), names_to = "col", values_to = "val")

  out_estimate <- .hlpr_get_col(input = out, col = "Estimate")
  out_pvalue <- .hlpr_get_col(input = out, col = "p_value")
  out_se <- .hlpr_get_col(input = out, col = "Std_Error")

  if (!show_cv) {
    out_estimate <- out_estimate[!(out_estimate$Variables %in% c("(Intercept)", cv)), -2]
    out_pvalue <- out_pvalue[!(out_pvalue$Variables %in% c("(Intercept)", cv)), -2]
    out_se <- out_se[!(out_se$Variables %in% c("(Intercept)", cv)), -2]
    model_list <- model_list[-1]
  }

  out <- list(coefficients = out_estimate, p.values = out_pvalue, std.errors = out_se, models = model_list)
  return(out)
}
