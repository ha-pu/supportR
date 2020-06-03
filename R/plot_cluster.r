plot_cluster <- function(model, var_cluster, iv, moderator, ci = 0.9, use_bootstrap = TRUE, name_mod = NULL, name_iv = NULL) {
  
  # to do: control class(var_cluster) %in% c(vector, matrix, data.frame, tibble?)
  
  # run checks ----
  if (!is.character(iv) | !is.character(moderator)) stop("iv and moderator must be character variables!")
  if (dim(model$model)[1] != length(var_cluster)) stop("dim(model$model)[1] must equal length(var_cluster)!")
  if (!is.numeric(ci) | ci >= 1) {
	ci <- 0.9
	warning("ci must be a numeric value < 1! Default for ci is 0.9.")
  }
  
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

    # get plot data ----
    data <- margins::cplot(model, x = moderator, dx = iv, what = "effect", data = model$model, level = ci, vcov = vcov_mat, draw = FALSE)

    # draw plot ----
    data_rug <- model$model
    data_rug$rug <- data_rug[,moderator]

    caption <- "Clustered standard errors"
    if (use_bootstrap) {
      caption <- paste0(caption, " with bootstrapping")
    }

	if (is.null(name_iv)) {
		name_iv <- iv
	}
	if (is.null(name_mod)) {
		name_mod <- moderator
	}

    out_plot <- ggplot2::ggplot(data, ggplot2::aes(x = xvals)) +
      ggplot2::geom_line(ggplot2::aes(y = yvals)) +
      ggplot2::geom_hline(yintercept = 0) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 25),
            plot.subtitle = ggplot2::element_text(size = 15),
            axis.title = ggplot2::element_text(size = 15),
            axis.text = ggplot2::element_text(size = 10))+
      ggplot2::geom_line(ggplot2::aes(y = upper), linetype = 2) +
      ggplot2::geom_line(ggplot2::aes(y = lower), linetype = 2) +
      ggplot2::geom_rug(data = data_rug, ggplot2::aes(rug), sides = "b") +
      ggplot2::labs(title = paste0("marginal effect ", name_iv), subtitle = paste0(ci * 100, "% confidence interval"), x = name_mod, y = "marginal effect", caption = caption)

    return(out_plot)
  } else {
	warning("Function currently only supports class lm!")
  }
  
  # to do: "glm" %in% class(model)
}
