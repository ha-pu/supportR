# model_summary generic function ----
model_summary <- function(input, ...) UseMethod("model_summary", input)

# model_summary mod_vcov method ----
model_summary.mod_vcov <- function(input, show = TRUE, sg = FALSE, ...) {
  
  # get data ----
  model <- input$model
  vcov_mat <- input$vcov_mat
  
  # compute se and p-values ----
  coef <- model$coefficients
  se <- lmtest::coeftest(model, vcov_mat)[,2]
  t_value <- coef / se
  p_value <- 2 * pt(-abs(t_value), df = model$df.residual)
  
  # export data ----
  out <- data.frame(Variables = names(coef),
                    Estimate = coef,
                    Std_Error = se,
                    t_value,
                    p_value,
                    row.names = NULL)
  class(out) <- c("model_summary", class(out))
  
  # prepare output ----
  if (show & sg) {
    warning("'show' == TRUE overrules 'sg' == TRUE")
  }
  if (show) {
    print(x = out)
  } else {
    if (sg) {
      out <- list(model, out$se)
    } else if (requireNamespace("tibble", quietly = TRUE)) {
      out <- tibble::as_tibble(out)
    }
    return(out)
  }
}

# model_summary lm and glm method ----
model_summary.lm <- model_summary.glm <- function(input, type = 0, var_cluster = NULL, show = TRUE, sg = FALSE, ...) {
  out <- model_vcov(model = input, type = type, var_cluster = var_cluster)
  out <- model_summary(input = out, show = FALSE)
  class(out) <- c("model_summary", class(out))
  
  # prepare output ----
  if (show & sg) {
    message("'show' == TRUE overrules 'sg' == TRUE")
  }
  if (show) {
    print(x = out)
  } else {
    if (sg) {
      out <- list(input, out$Std_Error)
    } else if (requireNamespace("tibble", quietly = TRUE)) {
      out <- tibble::as_tibble(out)
    }
    return(out)
  }
}

# model_summary lmer and glmer method ----
model_summary.lmerMod <- model_summary.glmerMod <- function(input, randfe = FALSE, show = TRUE, sg = FALSE, ...) {
  
  # compute se and p-values ----
  name <- attributes(input@pp$X)$dimnames[[2]]
  coef <- input@beta
  se <- sqrt(Matrix::diag(vcov(input)))
  t_value <- coef / se
  p_value <- 2 * pt(-abs(t_value), df = nrow(input@frame) - 1)
  out <- data.frame(Variables = name,
                    Estimate = coef,
                    Std_Error = se,
                    t_value,
                    p_value,
                    row.names = NULL)
  class(out) <- c("model_summary", class(out))
  
  # get random fixed effects ----
  if (randfe) {
    out_randfe <- lme4::ranef(input)
    out_randfe <- purrr::map(seq(length(out_randfe)), ~{
      out <- as.data.frame(out_randfe[[.x]])
      names(out) <- "Estimate"
      out$Group <- row.names(out)
      row.names(out) <- NULL
      out$Effect <- names(out_randfe)[[.x]]
      out <- out[,c(3:1)]
      return(out)
    })
    out_randfe <- dplyr::bind_rows(out_randfe)
    class(out_randfe) <- c("model_randfe", class(out_randfe))
  }
  
  
  # prepare output ----
  if (show & sg) {
    message("'show' == TRUE overrules 'sg' == TRUE")
  }
  if (show) {
    if (!randfe) {
      print(x = out)
    } else {
      purrr::map(list(Variables = out, RandFE = out_randfe), print)
    }
  } else {
    if (sg) {
      out <- list(input)
    } else if (requireNamespace("tibble", quietly = TRUE)) {
      out <- tibble::as_tibble(out)
      if (randfe) {
        out_randfe <- tibble::as_tibble(out_randfe)
      }
    } else {
      class(out) <- "data.frame"
      class(out_randfe) <- "data.frame"
    }
    if (!randfe) {
      return(out)
	} else if (sg & randfe) {
	  return(out)
	  message("'sg' == TRUE overrules 'randfe' == TRUE")
    } else {
      out <- list(Variables = out, RandFE = out_randfe)
      return(out)
    }
  }
}
