# modular_regressions ----
modular_regressions <- function(type = "glm", fam = "gaussian", dv, cv = NULL, iv, mv, fe = NULL, robust_type = 0, var_cluster = NULL, data, full_model = TRUE, show = TRUE, show_cv = FALSE) {
  
  if (type == "glm") {
    model_parameters <- list(dv = dv, cv = cv, iv = iv, mv = mv, data = data, full_model = full_model)
    class(model_parameters) <- c("param_glm", class(model_parameters))
    attr(model_parameters, "family") <- fam
  } else if (type == "glmer") {
    model_parameters <- list(dv = dv, cv = cv, iv = iv, mv = mv, fe = fe, data = data, full_model = full_model)
    class(model_parameters) <- c("param_glmer", class(model_parameters))
    attr(model_parameters, "family") <- fam
  }

  out <- .hlpr_modular_models(parameters = model_parameters)

  if (robust_type != 0) {
    var_cluster <- as.data.frame(data)[,var_cluster]
  }
  
  out <- .hlpr_modular_report(model_list = out, cv = cv, show_cv = show_cv, robust_type = robust_type, var_cluster = var_cluster)

  if (show) {
    if (requireNamespace("stargazer", quietly = TRUE)) {
      if (show_cv) {
        std_errors <- vector(mode = "list", length = ncol(out$std.errors) - 1)
        for (i in seq(ncol(out$std.errors) - 1)) {
          std_errors[[i]] <- na.omit(as.data.frame(out$std.errors)[,i + 1])
        }
        stargazer::stargazer(out$models, se = std_errors, type = "text", report = "vc*p", model.numbers = FALSE, intercept.bottom = FALSE, single.row = TRUE)
      } else {
        warning("Stargazer output requires 'show_cv' == TRUE!")
        if (requireNamespace("knitr", quietly = TRUE)) {
          lapply(out[-4], function(x) knitr::kable(x, digits = 3))
        } else {
          lapply(out[-4], print)
        }
      }
    } else if (requireNamespace("knitr", quietly = TRUE)) {
      lapply(out[-4], function(x) knitr::kable(x, digits = 3))
    } else {
      lapply(out[-4], print)
    }
  } else {
    return(out)
  }
}
