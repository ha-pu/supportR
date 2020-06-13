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
  
  out <- modular_models(parameters = model_parameters)
  
  if (robust_type != 0) {
    var_cluster <- as.data.frame(data)[,var_cluster]
  }
  
  out <- modular_report(model_list = out, cv = cv, show_cv = show_cv, robust_type = robust_type, var_cluster = var_cluster)
  
  if (show) {
    if (requireNamespace("knitr", quietly = TRUE)) {
      lapply(out, function(x) knitr::kable(x, digits = 3))
    } else {
      lapply(out, print)
    }
  } else {
    return(out)
  }
}

# modular_models generic function ----
modular_models <- function(parameters, ...)  UseMethod("modular_models", parameters)

# modular_models param_glm method ----
modular_models.param_glm <- function(parameters, ...) {
  dv <- parameters$dv
  cv <- parameters$cv
  iv <- parameters$iv
  mv <- parameters$mv
  data <- parameters$data
  
  if (is.null(cv)) {
    cv <- 1
  }
  
  out <- vector(mode = "list", length = 3 + length(mv) * 2)
  form_cv <- formula(paste0(dv, "~", paste0(cv, collapse = "+")))
  out[[1]] <- glm(formula = form_cv, data = data, family = attr(parameters, "family"))
  out <- .get_models(input = out, dv = dv, cv = cv, iv = iv, mv = mv, full_model = full_model)
  
  return(out)
}

# modular_models param_glmer method ----
modular_models.param_glmer <- function(parameters, ...) {
  dv <- parameters$dv
  cv <- parameters$cv
  iv <- parameters$iv
  mv <- parameters$mv
  fe <- paste0("(1|", parameters$fe, ")")
  data <- parameters$data
  
  if (!is.null(cv)) {
    cv <- c(cv, fe)
  } else {
    cv <- fe
  }
  
  out <- vector(mode = "list", length = 3 + length(mv) * 2)
  form_cv <- formula(paste0(dv, "~", paste0(cv, collapse = "+")))
  if (attr(parameters, "family") == "gaussian") {
    out[[1]] <- lme4::lmer(formula = form_cv, data = data)
  } else {
    out[[1]] <- lme4::glmer(formula = form_cv, data = data, fam = attr(parameters, "family"))
  }
  out <- .get_models(input = out, dv = dv, cv = cv, iv = iv, mv = mv, full_model = full_model)
  
  return(out)
}

# refactor functions for modular_model ----
.get_models <- function(input, dv, cv, iv, mv, full_model) {
  out <- input
  form_iv <- formula(paste0(dv, "~", paste0(cv, collapse = "+"), "+", iv))
  out[[2]] <- update(out[[1]], formula. = form_iv)
  
  for (i in seq(length(mv))) {
    j <- i * 2 + 1
    form_mv <- formula(paste0(dv, "~", paste0(cv, collapse = "+"), "+", iv, "+", mv[i]))
    out[[j]] <- update(out[[1]], formula. = form_mv)
    j <- j + 1
    form_mv <- formula(paste0(dv, "~", paste0(cv, collapse = "+"), "+", iv, "+", mv[i], "+", paste0(c(iv, mv[i]), collapse = ":")))
    out[[j]] <- update(out[[1]], formula. = form_mv)
  }
  
  if (full_model) {
    form_full <- formula(paste0(dv, "~", paste0(cv, collapse = "+"), "+", iv, "+", paste0(mv, collapse = "+"), "+", .paste1(element1 = iv, element2 = mv, sep = ":", collapse = "+")))
    out[[length(out)]] <- update(out[[1]], formula. = form_full)
  } else {
    out <- out[-length(out)]  
  }
  return(out)
}

# modular_report function ----
modular_report <- function(model_list, cv, show_cv, robust_type, var_cluster) {
  out <- lapply(seq(length(model_list)), function(x) {
    out <- model_summary(input = model_list[[x]], type = robust_type, var_cluster = var_cluster, show = FALSE)
    out$mod <- paste0("mod_", x)
    return(out)
  })
  
  out <- dplyr::bind_rows(out)
  out <- tidyr::pivot_longer(out, -c(Coefficients, mod), names_to = "col", values_to = "val")
  
  out_estimate <- .get_col(input = out, col = "Estimate")
  out_pvalue <- .get_col(input = out, col = "p_value")
  out_se <- .get_col(input = out, col = "Std_Error")
  
  if (!show_cv) {
    out_estimate <- out_estimate[!(out_estimate$Coefficients %in% c("(Intercept)", cv)), -2]
    out_pvalue <- out_pvalue[!(out_pvalue$Coefficients %in% c("(Intercept)", cv)), -2]
	out_se <- out_se[!(out_se$Coefficients %in% c("(Intercept)", cv)), -2]
  }
  
  out <- list(coefficients = out_estimate, p.values = out_pvalue, str.errors = out_se)
  return(out)
}

# refactor functions for modular_report ----
.get_col <- function(input, col) {
  out <- input[input$col == col,]
  out <- tidyr::pivot_wider(out, names_from = mod, values_from = val)
  out <- out[,-2]
  return(out)
}