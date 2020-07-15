\name{model_summary}

\alias{model_summary}
\alias{model_summary.mod_vcov}
\alias{model_summary.lm}
\alias{model_summary.glm}
\alias{model_summary.lmerMod}
\alias{model_summary.glmerMod}

\title{Prepare model summary}

\description{
  The function \code{model_summary} computes coefficient estimates for linear regression models or linear fixe effects models. For linear models, the user can provide a cluster variable to cluster the standard errors and can select to use bootstraping for clustering.
}

\usage{
  model_summary(input, type, var_cluster, randfe = FALSE, show = TRUE, sg = FALSE)
}

\arguments{
  \item{input}{Object of class "mod_vcov", linear model of class "lm" or "glm", or linear fixed effects model of class "lmerMod" or "glmerMod"}
  \item{type}{Indicator which type of standard errors is to be computed: 0 - no clustering, 1 - clustering using \code{var_cluster}, 2 - clustering using \code{var_cluster} with bootstraping}
  \item{var_cluster}{Vector, matrix, or data.frame containing the variables that are used for clustering}
  \item{randfe}{\code{logical} variable indicating whether random fixed effects are provided for models of class "lmerMod" or "glmerMod"}
  \item{show}{\code{logical} variable indicating whether output is printed to consolde or returned as \code{data.frame}}
  \item{sg}{\code{logical} variable indicating whether output is printed in html format or as standard output}
}

\details{
  For input of class "lm" or "glm" the options \code{type == 0},  \code{type == 1}, and \code{type == 2} control how the variance-covariance matrix for standard error estimation is computed.
}

\value{
  Coefficient estimates for the model provided as \code{"input"} based on model class and selection in \code{type}.

  When the option \code{show} is set to \code{TRUE} the output is printed to the console. If the package \code{knitr} is available, the output is printed using \code{knitr::kable}.

  When \code{sg} is set to \code{TRUE} the output is printed in html format (this overrules \code{show == FALSE}). When \code{show} is set to \code{FALSE} the output is returned as \code{data.frame} or \code{tibble} (when package \code{tibble} is available).

  For models of class "lmerMod" and "glmerMod" and \code{randfe == TRUE}, random fixed effects are either  printed (\code{show == TRUE}) or returned as part of a named list containing \code{Variables} = coefficient estimates and \code{RandFE} containing the random fixed effects.
}

\seealso{
  \code{\link{model_vcov}}
}

\examples{
  data <- supportR::create_data()

  mod1 <- lm(firm_value ~ profit + cogs + rnd + competition * board_size, data = data)
  mod1_vcov <- model_vcov(model = mod1, type = 1, var_cluster = data$country)
  model_summary(input = mod1_vcov, show = TRUE, sg = FALSE)
  model_summary(input = mod1, type = 0, var_cluster = NULL, show = TRUE, sg = FALSE)

  mod2 <- glm(female_ceo ~ profit + cogs + rnd + ceo_age * board_size, data = data, family = "binomial")
  model_summary(input = mod2, type = 2, var_cluster = data$industry, show = TRUE, sg = FALSE)

  mod3 <- lme4::lmer(firm_value ~ profit + cogs + rnd + competition * board_size + (1 | country), data = data)
  model_summary(input = mod3, randfe = TRUE, show = FALSE, sg = FALSE)

  mod4 <- lme4::glmer(female_ceo ~ profit + cogs + rnd + ceo_age * board_size + (1 | industry), data = data, family = "binomial")
  model_summary(input = mod4, randfe = FALSE, show = FALSE, sg = TRUE)
}
