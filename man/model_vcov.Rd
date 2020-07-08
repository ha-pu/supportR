\name{model_vcov}

\alias{model_vcov}
\alias{model_vcov.lm}
\alias{model_vcov.glm}

\title{Compute model variance-covariance matrix}

\description{
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  model_vcov(model, ...)
}

\arguments{
  \item{model}{
  %%     ~~Describe \code{model} here~~
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
  \code{\link[multiwayvcov]{cluster.vcov}}, \code{\link[multiwayvcov]{cluster.boot}}
}

\examples{
  data <- supportR::create_data()

  mod1 <- lm(firm_value ~ profit + cogs + rnd + competition * board_size, data = data)
  model_vcov(model = mod1, type = 0, var_cluster = NULL)
  model_vcov(model = mod1, type = 1, var_cluster = data$country)

  mod2 <- glm(female_ceo ~ profit + cogs + rnd + ceo_age * board_size, data = data, family = "binomial")
  model_vcov(model = mod2, type = 2, var_cluster = data$industry)
}
