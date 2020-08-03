\name{plot_interaction}

\alias{plot_interaction}
\alias{plot_interaction.mod_vcov}
\alias{plot_interaction.lm}
\alias{plot_interaction.glm}
\alias{plot_interaction.lmerMod}
\alias{plot_interaction.glmerMod}

\title{Plot interaction effects for regression models}

\description{
  The function \code{plot_interaction} provides interaction plots for linear regression models or linear fixed effects models. For linear models, the user can provide a cluster variable to cluster the standard errors and can select to use bootstraping for clustering.
}

\usage{
  plot_interaction(input, iv, moderator, ci = 0.9, type = 0, var_cluster = NULL)
}

\arguments{
  \item{input}{Object of class "mod_vcov", linear model of class "lm" or "glm", or linear fixed effects model of class "lmerMod" or "glmerMod"}
  \item{iv}{\code{character} giving the name of the independt variable for which the interaction effect is plotted - for \code{input} is class "lmerMod" or "glmerMod" a logical iv must be indicated as "variableTRUE""}
  \item{moderator}{\code{character} giving the name of the variable that moderates the interaction effect - the moderator variable mustnot be of class "logical"}
  \item{ci}{Confidence interval shown in interaction plot - must be numeric between 0 and 1}
  \item{type}{Indicator which type of standard errors is to be computed: 0 - no clustering, 1 - clustering using \code{var_cluster}, 2 - clustering using \code{var_cluster} with bootstraping}
  \item{var_cluster}{Vector, matrix, or data.frame containing the variables that are used for clustering}
}

\details{
  For input of class "lm" or "glm" the function uses \code{cplot} from the \code{margins} package to prepare the interaction plot. For input of class "lmerMod" and "glmerMod" the funciton uses \code{interplot} from the \code{interplot} package to prepare the interaction plot. For input of class "lm" or "glm" the options \code{type == 0},  \code{type == 1}, and \code{type == 2} control how the variance-covariance matrix for standard error estimation is computed. The options \code{type == 1} and \code{type == 2} require a cluster variable defined in \code{var_cluster}.
}

\value{
  Interaction plot as \code{ggplot2} object
}

\seealso{
  \code{\link[margins]{cplot}}, \code{\link[interplot]{interplot}}, \code{\link[ggplot2]{ggplot}}
}

\examples{
  data <- supportR::create_data()

  mod1 <- lm(firm_value ~ profit + cogs + rnd + competition * board_size, data = data)
  mod1_vcov <- model_vcov(model = mod1, type = 1, var_cluster = data$country)
  plot_interaction(input = mod1_vcov, iv = "competition", moderator = "board_size", ci = 0.9)
  plot_interaction(input = mod1, iv = "competition", moderator = "board_size", ci = 0.9, type = 0, var_cluster = NULL)


  mod2 <- glm(female_ceo ~ profit + cogs + rnd + ceo_age * board_size, data = data, family = "binomial")
  plot_interaction(input = mod2, iv = "ceo_age", moderator = "board_size", ci = 0.9, type = 2, var_cluster = data$industry)

  mod3 <- lme4::lmer(firm_value ~ profit + cogs + rnd + competition * board_size + (1 | country), data = data)
  plot_interaction(input = mod3, iv = "competition", moderator = "board_size", ci = 0.9)

  mod4 <- lme4::glmer(female_ceo ~ profit + cogs + rnd + ceo_age * board_size + (1 | industry), data = data, family = "binomial")
  plot_interaction(input = mod4, iv = "ceo_age", moderator = "board_size", ci = 0.9)
}
