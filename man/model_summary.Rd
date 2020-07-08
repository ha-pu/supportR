\name{model_summary}

\alias{model_summary}
\alias{model_summary.mod_vcov}
\alias{model_summary.lm}
\alias{model_summary.glm}
\alias{model_summary.lmerMod}
\alias{model_summary.glmerMod}

\title{Prepare model summary}

\description{
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}

\usage{
  model_summary(input, ...)
}

\arguments{
  \item{input}{
  %%     ~~Describe \code{input} here~~
  }
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
  model_summary(input = mod3, show = FALSE, sg = FALSE)

  mod4 <- lme4::glmer(female_ceo ~ profit + cogs + rnd + ceo_age * board_size + (1 | industry), data = data, family = "binomial")
  model_summary(input = mod4, show = FALSE, sg = TRUE)
}
