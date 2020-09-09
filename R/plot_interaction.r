#' @title Plot interaction effects for regression models
#'
#' @aliases
#' plot_interaction
#' plot_interaction.mod_vcov
#' plot_interaction.lm
#' plot_interaction.glm
#' plot_interaction.lmerMod
#' plot_interaction.glmerMod
#'
#' @description
#' The function \code{plot_interaction} provides interaction plots for
#' linear regression models or linear fixed effects models. For linear
#' models, the user can provide a cluster variable to cluster the standard
#' errors and can select to use bootstraping for clustering.
#'
#' @details
#' For input of class "lm" or "glm" the function uses \code{cplot} from
#' the \code{margins} package to prepare the interaction plot. For input
#' of class "lmerMod" and "glmerMod" the funciton uses \code{interplot}
#' from the \code{interplot} package to prepare the interaction plot. For
#' input of class "lm" or "glm" the options \code{type == 0},
#' \code{type == 1}, and \code{type == 2} control how the variance-covariance
#' matrix for standard error estimation is computed. The options
#' \code{type == 1} and \code{type == 2} require a cluster variable
#' defined in \code{var_cluster}.
#'
#' @param input}{Object of class "mod_vcov", linear model of class "lm" or
#' "glm", or linear fixed effects model of class "lmerMod" or "glmerMod"
#' @param iv}{\code{character} giving the name of the independt variable
#' for which the interaction effect is plotted - for \code{input} is class
#' "lmerMod" or "glmerMod" a logical iv must be indicated as "variableTRUE"
#' @param moderator}{\code{character} giving the name of the moderator
#' variable (the x-axis of the plot) - the moderator variable mustnot be
#' of class "logical"
#' @param name_iv}{\code{character} giving the label for the independent
#' variable used for the title of the plot
#' @param name_mod}{\code{character} giving the label for the moderator
#' variable used for the x-axis of the plot
#' @param ci}{Confidence interval shown in interaction plot - must be
#' numeric between 0 and 1
#' @param type}{Indicator which type of standard errors is to be
#' computed: 0 - no clustering, 1 - clustering using \code{var_cluster},
#' 2 - clustering using \code{var_cluster} with bootstraping
#' @param var_cluster}{Vector, matrix, or data.frame containing the
#' variables that are used for clustering
#'
#' @return Interaction plot as \code{ggplot2} object
#'
#' @seealso \code{\link[margins]{cplot}}, \code{\link[interplot]{interplot}}, \code{\link[ggplot2]{ggplot}}
#'
#' @examples
#' data <- create_data()
#'
#' mod1 <- lm(ebit ~ advertising + assets + country + international * rnd, data = data)
#' mod1_vcov <- model_vcov(model = mod1, type = 1, var_cluster = data$country)
#' plot_interaction(input = mod1_vcov, iv = "international", moderator = "rnd", ci = 0.9)
#' plot_interaction(
#'   input = mod1, iv = "international", moderator = "rnd",
#'   ci = 0.9, type = 0, var_cluster = NULL
#' )
#'
#' mod2 <- glm(female_ceo ~ advertising + assets + country + international * rnd,
#'   data = data, family = "binomial"
#' )
#' plot_interaction(
#'   input = mod2, iv = "international", moderator = "rnd",
#'   ci = 0.9, type = 2, var_cluster = data$country
#' )
#'
#' mod3 <- lme4::lmer(ebit ~ advertising + assets + country + international * rnd + (1 | country),
#'   data = data
#' )
#' plot_interaction(input = mod3, iv = "internationalTRUE", moderator = "rnd", ci = 0.9)
#'
#' mod4 <- lme4::glmer(female_ceo ~ advertising + assets + country + international * rnd +
#'   (1 | country), data = data, family = "binomial")
#' plot_interaction(input = mod4, iv = "internationalTRUE", moderator = "rnd", ci = 0.9)
#' @rdname plot_interaction
#' @export
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_rug
#' @importFrom ggplot2 labs
#' @importFrom graphics rug
#' @importFrom interplot interplot
#' @importFrom margins cplot

plot_interaction <- function(input, ...) UseMethod("plot_interaction", input)

#' @rdname plot_interaction
#' @method plot_interaction mod_vcov
#' @export

plot_interaction.mod_vcov <- function(input, iv, moderator, name_iv = iv, name_mod = moderator, ci = 0.9, ...) {

  # run checks ----
  if (!is.character(iv) | !is.character(moderator)) stop("iv and moderator must be character values!")
  if (!is.character(name_iv)) {
    name_iv <- iv
    warning("name_iv must be a character value! Use default value iv for name_iv.")
  }
  if (!is.character(name_mod)) {
    name_mod <- iv
    warning("name_mod must be a character value! Use default value moderator for name_mod.")
  }
  if (!is.numeric(ci) | ci >= 1) {
    ci <- 0.9
    warning("ci must be a numeric value < 1! Use default value 0.9 for ci.")
  }

  # get data ----
  model <- input$model
  vcov_mat <- input$vcov_mat
  data <- cplot(model, x = moderator, dx = iv, what = "effect", data = model$model, level = ci, vcov = vcov_mat, draw = FALSE)

  # draw plot ----
  data_rug <- model$model
  data_rug$rug <- data_rug[, moderator]

  if (attr(input, "type") == "standard") {
    caption <- NULL
  } else if (attr(input, "type") == "robust") {
    caption <- "Clustered standard errors"
  } else if (attr(input, "type") == "robust_boot") {
    caption <- "Clustered standard errors with bootstrapping"
  }

  out <- ggplot(data, aes(x = xvals)) +
    geom_line(aes(y = yvals)) +
    geom_hline(yintercept = 0) +
    geom_line(aes(y = upper), linetype = 2) +
    geom_line(aes(y = lower), linetype = 2) +
    geom_rug(data = data_rug, aes(rug), sides = "b") +
    labs(title = paste0("marginal effect ", name_iv), subtitle = paste0(ci * 100, "% confidence interval"), x = name_mod, y = "marginal effect", caption = caption)

  return(out)
}

#' @rdname plot_interaction
#' @method plot_interaction lm
#' @export

plot_interaction.lm <- function(input, iv, moderator, name_iv = iv, name_mod = moderator, ci = 0.9, type = 0, var_cluster = NULL, ...) {
  out <- model_vcov(model = input, type = type, var_cluster = var_cluster)
  out <- plot_interaction(input = out, iv = iv, moderator = moderator, name_iv = name_iv, name_mod = name_mod, ci = ci)

  return(out)
}

#' @rdname plot_interaction
#' @method plot_interaction glm
#' @export

plot_interaction.glm <- plot_interaction.lm

#' @rdname plot_interaction
#' @method plot_interaction lmerMod
#' @export

plot_interaction.lmerMod <- function(input, iv, moderator, name_iv = iv, name_mod = moderator, ci = 0.9, ...) {

  # run checks ----
  if (!is.character(iv) | !is.character(moderator)) stop("iv and moderator must be character values!")
  if (!is.character(name_iv)) {
    name_iv <- iv
    warning("name_iv must be a character value! Use default value iv for name_iv.")
  }
  if (!is.character(name_mod)) {
    name_mod <- iv
    warning("name_mod must be a character value! Use default value moderator for name_mod.")
  }
  if (!is.numeric(ci) | ci >= 1) {
    ci <- 0.9
    warning("ci must be a numeric value < 1! Use default value 0.9 for ci.")
  }

  # draw plot ----
  out <- interplot(input, var1 = iv, var2 = moderator, ci = ci, hist = TRUE) +
    labs(title = paste0("marginal effect ", name_iv), subtitle = paste0(ci * 100, "% confidence interval"), x = name_mod, y = "marginal effect", caption = "Linear fixed effects model") +
    geom_hline(yintercept = 0)

  return(out)
}

#' @rdname plot_interaction
#' @method plot_interaction glmerMod
#' @export

plot_interaction.glmerMod <- plot_interaction.lmerMod
