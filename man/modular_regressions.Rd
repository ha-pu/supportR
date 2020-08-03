\name{modular_regressions}
\alias{modular_regressions}

\title{Modular regressions}

\description{
  The function allows to automatically specify a number of regression models based on an input of a dependent variable, independent variables, control variables, interaction terms, and model structure.
}

\usage{
  modular_regressions(type = "glm", fam = "gaussian", dv, cv = NULL, iv, mv, fe = NULL, robust_type = 0, var_cluster = NULL, data, full_model = TRUE, show = TRUE, show_cv = FALSE)
}

\arguments{
  \item{type}{\code{character} variable indicating the model type, takes "glm" or "glmer"}
  \item{fam}{\code{character} variable indicating the model familiy, takes: "gaussian" or "binomial"}
  \item{dv}{\code{character} variable indicating the name of the dependt variable}
  \item{cv}{\code{character} variable or vector thereof indicating the name of the control variables, take \code{NULL} as input}
  \item{iv}{\code{character} variable indicating the name of the independt variable}
  \item{mv}{\code{character} variable or vector thereof indicating the name of the moderator variables}
  \item{fe}{\code{character} variable or vector thereof indicating the name of the fixed effect variables}
  \item{robust_type}{Indicator which type of standard errors is to be computed: 0 - no clustering, 1 - clustering using \code{var_cluster}, 2 - clustering using \code{var_cluster} with bootstraping}
  \item{var_cluster}{Vector, matrix, or data.frame containing the variables that are used for clustering}
  \item{data}{\code{data.frame} used to run the regression models - all variables are taken from \code{data}}
  \item{full_model}{\code{logical} variable whether a full model, including all indicatred interaction terms, should be computed}
  \item{show}{\code{logical} variable indicating whether output is printed to consolde or returned as \code{data.frame}}
  \item{show_cv}{\code{logical} variable indicating whether output should include intercept and control variables}
}

\details{
  The function runs a series of models of \code{type} and \code{familiy}. When \code{type} == "glmer" is selected, the variables defined in \code{fe} are used to compute random intercepts, i.e. "(1 | fe)". First, the function runs a controls only model regressing \code{cv} on \code{dv}. Next, a base model is computed as \code{dv} = f(\code{cv} + \code{iv}). Then a series of interaction models with all combinations of \code{iv} and \code{mv} is computed. If \code{full_model} is set to \code{TRUE}, a full model with all combinations of \code{iv} and \code{mv} in it is added.
  For \code{type} "glm" the options \code{robust_type == 0},  \code{robust_type == 1}, and \code{robust_type == 2} control how the variance-covariance matrix for standard error estimation is computed. The options \code{robust_type == 1} and \code{robust_type == 2} require a cluster variable defined in \code{var_cluster}.
  \code{type} "glmer" requires one or more variables to compute fixed effects defined in \code{fe}.
}

\value{
  The output is a list of three elements, named "coefficents", "p.values", and "std.errors". Each of the list elements shows the coeffient estimates, p-values, and standard errors for the computed models, respectively.

  When the option \code{show} is set to \code{TRUE} the output is printed to the console. If the package \code{knitr} is available, the output is printed using \code{knitr::kable}. When \code{show} and \code{show_cv} are set to \code{TRUE} the output is printed using package \code{stargazer}.
}

\references{
  \code{\link{model_vcov}}, \code{\link[lme4]{lmer}}, \code{\link[lme4]{glmer}}
}

\examples{
  data <- supportR::create_data()

  modular_regressions(type = "glm", fam = "gaussian", dv = "firm_value", cv = c("rnd", "competition", "quality"), iv = "profit", mv = c("cogs", "board_size"), robust_type = 0, data = data, full_model = TRUE, show = TRUE, show_cv = FALSE)
  modular_regressions(type = "glm", fam = "gaussian", dv = "firm_value", iv = "profit", mv = c("cogs", "board_size"), robust_type = 2, var_cluster = "industry", data = data, full_model = FALSE, show = TRUE, show_cv = FALSE)
  modular_regressions(type = "glmer", fam = "gaussian", dv = "firm_value", cv = c("rnd", "competition", "quality"), iv = "profit", mv = c("cogs", "board_size"), fe = "industry", data = data, full_model = TRUE, show = FALSE, show_cv = FALSE)
  
  modular_regressions(type = "glm", fam = "binomial", dv = "female_ceo", cv = c("rnd", "competition", "quality"), iv = "profit", mv = c("cogs", "board_size"), robust_type = 0, var_cluster = NULL, data = data, full_model = TRUE, show = TRUE, show_cv = TRUE)
  modular_regressions(type = "glm", fam = "binomial", dv = "female_ceo", cv = c("rnd", "competition", "quality"), iv = "profit", mv = c("cogs", "board_size"), robust_type = 2, var_cluster = "industry", data = data, full_model = FALSE, show = TRUE, show_cv = FALSE)
  modular_regressions(type = "glmer", fam = "binomial", dv = "female_ceo", iv = "profit", mv = c("cogs", "board_size"), fe = "industry", data = data, full_model = TRUE, show = TRUE, show_cv = FALSE)

}
