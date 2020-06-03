plot_lmer <- function(model, iv, moderator, ci = 0.9) {
  
  # run checks ----
  if (!is.character(iv) | !is.character(moderator)) stop("iv and moderator must be character variables!")
  if (!is.numeric(ci) | ci >= 1) {
	ci <- 0.9
	warning("ci must be a numeric value < 1! Default for ci is 0.9.")
  }
  
  # run for "lmerMod" %in% class(model) ----
  if ("lmerMod" %in% class(model)) {

        # draw plot ----
    out_plot <- interplot::interplot(model, var1 = iv, var2 = moderator, ci = ci, hist = TRUE) +
      ggplot2::labs(title = paste0("marginal effect ", iv), subtitle = paste0(ci * 100, "% confidence interval"), x = moderator, y = "marginal effect", caption = "Linear fixed effects model") +
      ggplot2::geom_hline(yintercept = 0)

    return(out_plot)
  } else {
	warning("Function currently only supports class lmerMod!")
  }
  
  # to do: "glmerMod" %in% class(model)
}
