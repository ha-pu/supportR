# get_p generic function ----
get_p <- function(model, ...) UseMethod("get_p", model)

# method for class "lm" ----
get_p.lm <- function(model, var_cluster = NULL, cluster_boot = FALSE, show = TRUE, stargazer = FALSE, ...) {
  
  # run checks ----
  if (!is.null(var_cluster) & nrow(model$model) != length(var_cluster)) stop("When using var_cluster, nrow(model$model) must equal length(var_cluster)!")
  
  # compute variance covariance matrix ----
  mat <- vcov(model)
  if(is.null(var_cluster)) {
    vcov_mat <- mat
  } else {
    if (!cluster_boot) {
      vcov_mat <- multiwayvcov::cluster.vcov(model, var_cluster)
    } else if (cluster_boot) {
      vcov_mat <- multiwayvcov::cluster.boot(model, var_cluster, R = 1000, boot_type = "wild", wild_type = function() sample(c(-1, 1), 1))
    }
    colnames(vcov_mat) <- colnames(mat)
    row.names(vcov_mat) <- row.names(mat)
  }
  
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
  
  if (show) {
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
}

# method for class "glm" ----
get_p.glm <- get_p.lm

# method for class "lmerMod" ----
get_p.lmerMod <- function(model, show = TRUE, ...) {
  
  # get data ----
  name <- attributes(model@pp$X)$dimnames[[2]]
  coef <- model@beta
  se <- sqrt(Matrix::diag(vcov(model)))
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
  if (show) {
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
  
}

# method for class "glmerMod" ----
get_p.glmerMod <- get_p.lmerMod
