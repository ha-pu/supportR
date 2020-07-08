\name{plot_interaction}

\alias{plot_interaction}
\alias{plot_interaction.mod_vcov}
\alias{plot_interaction.lm}
\alias{plot_interaction.glm}
\alias{plot_interaction.lmerMod}
\alias{plot_interaction.glmerMod}

\title{Plot interaction effects}

\description{
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}

\usage{
plot_interaction(input, ...)
}

\arguments{
  \item{input}{
  %%     ~~Describe \code{input} here~~
  }
  # moderator cannot be logical!!!
  # for plot_interaction.lmerMod / .glmerMod -> logical iv must be 'variableTRUE'
  \item{\dots}{
  %%     ~~Describe \code{\dots} here~~
  }
}

\details{
  %%  ~~ If necessary, more details than the description above ~~
}

\value{
  %%  ~Describe the value returned
  %%  If it is a LIST, use
  %%  \item{comp1 }{Description of 'comp1'}
  %%  \item{comp2 }{Description of 'comp2'}
  %% ...
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
