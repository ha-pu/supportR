\name{model_vcov}

\alias{model_vcov}
\alias{model_vcov.lm}
\alias{model_vcov.glm}

\title{Compute model variance-covariance matrix}

\description{
  The function \code{model_vcov} computes a variance-covariance matrix for a linear regression model. The user can provide a cluster variable to cluster the variance-covariance matrix and can select to use bootstraping for clustering. The output can serve as input for the \code{model_summary} and \code{plot_interaction} functions.
}
\usage{
  model_vcov(model, type, var_cluster)
}

\arguments{
  \item{model}{Linear regression model for which the variance-covariance matrix is to be computed, requires class "lm" or "glm"}
  \item{type}{Indicator which type of variance-covariance matrix is to be computed: 0 - no clustering, 1 - clustering using \code{var_cluster}, 2 - clustering using \code{var_cluster} with bootstraping}
  \item{var_cluster}{Vector, matrix, or data.frame containing the variables that are used for clustering}
}

\details{
  The option \code{type == 0} uses \code{vcov} from the \code{base} package. When \code{type == 1} is selected, the variance-covariance matrix is computed using the \code{cluster.vcov} command from the \code{multiwayvcov} package: \code{multiwayvcov::cluster.vcov(model, var_cluster)}. For \code{type == 2}, the variance-covariance matrix is computed using the \code{cluster.boot} command from the \code{multiwayvcov} package: \code{multiwayvcov::cluster.boot(model, var_cluster, R = 1000, boot_type = "wild", wild_type = function() sample(c(-1, 1), 1))}.
}

\value{
  A list of class ("mod_vcov" "list") containing two items:
  \item{model}{The linear regression model supplied as input}
  \item{vcov_mat}{A variance-covariance matrix for the linear regression model supplied in the function}
  For output's attributes depend on the \code{type} selected in the function call:
  \item{\code{type == 0}}{The attribute "type" is set to "standard"}
  \item{\code{type == 1}}{The attribute "type" is set to "robust", the attribute "var_cluster" takes the value supplied as variable \code{var_cluster}}
  \item{\code{type == 2}}{The attribute "type" is set to "robust_boot", the attribute "var_cluster" takes the value supplied as variable \code{var_cluster}}
}

\seealso{
  \code{\link[multiwayvcov]{cluster.vcov}}, \code{\link[multiwayvcov]{cluster.boot}}
}

\examples{
  data <- supportR::create_data()

  mod1 <- lm(ebit ~ advertising + assets + international + revenue * rnd, data = data)
  model_vcov(model = mod1, type = 0, var_cluster = NULL)
  model_vcov(model = mod1, type = 1, var_cluster = data$country)

  mod2 <- glm(female_ceo ~ advertising + assets + international + revenue * rnd,
  data = data, family = "binomial")
  model_vcov(model = mod2, type = 2, var_cluster = data$country)
}
