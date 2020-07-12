# plot_interaction generic function ----
plot_interaction <- function(input, ...) UseMethod("plot_interaction", input)

# plot_interaction mod_vcov method ----
plot_interaction.mod_vcov <- function(input, iv, moderator, ci = 0.9, ...) {
  
  # run checks ----
  if (!is.character(iv) | !is.character(moderator)) stop("iv and moderator must be character variables!")
  if (!is.numeric(ci) | ci >= 1) {
    ci <- 0.9
    warning("ci must be a numeric value < 1! Use default value for ci 0.9.")
  }
  
  # get data ----
  model <- input$model
  vcov_mat <- input$vcov_mat
  data <- margins::cplot(model, x = moderator, dx = iv, what = "effect", data = model$model, level = ci, vcov = vcov_mat, draw = FALSE)
  
  # draw plot ----
  data_rug <- model$model
  data_rug$rug <- data_rug[,moderator]
  
  if (attr(input, "type") == "standard") {
    caption <- NULL
  } else if (attr(input, "type") == "robust") {
    caption <- "Clustered standard errors"
  } else if (attr(input, "type") == "robust_boot") {
    caption <- "Clustered standard errors with bootstrapping"
  }
  
  out <- ggplot2::ggplot(data, ggplot2::aes(x = xvals)) +
    ggplot2::geom_line(ggplot2::aes(y = yvals)) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_line(ggplot2::aes(y = upper), linetype = 2) +
    ggplot2::geom_line(ggplot2::aes(y = lower), linetype = 2) +
    ggplot2::geom_rug(data = data_rug, ggplot2::aes(rug), sides = "b") +
    ggplot2::labs(title = paste0("marginal effect ", iv), subtitle = paste0(ci * 100, "% confidence interval"), x = moderator, y = "marginal effect", caption = caption)
  
  return(out)
}

# plot_interaction lm and glm method ----
plot_interaction.lm <- plot_interaction.glm <- function(input, iv, moderator, ci = 0.9, type = 0, var_cluster = NULL, ...) {
  out <- model_vcov(model = input, type = type, var_cluster = var_cluster)
  out <- plot_interaction(input = out, iv = iv, moderator = moderator, ci = ci)
  
  return(out)
}

# plot_interaction lmerMod and glmerMod method ----
plot_interaction.lmerMod <- plot_interaction.glmerMod <- function(input, iv, moderator, ci = 0.9, ...) {
  
  # run checks ----
  if (!is.character(iv) | !is.character(moderator)) stop("iv and moderator must be character variables!")
  if (!is.numeric(ci) | ci >= 1) {
    ci <- 0.9
    warning("ci must be a numeric value < 1! Use default value for ci 0.9.")
  }
  
  # draw plot ----
  out <- interplot::interplot(input, var1 = iv, var2 = moderator, ci = ci, hist = TRUE) +
    ggplot2::labs(title = paste0("marginal effect ", iv), subtitle = paste0(ci * 100, "% confidence interval"), x = moderator, y = "marginal effect", caption = "Linear fixed effects model") +
    ggplot2::geom_hline(yintercept = 0)
  
  return(out)
}
