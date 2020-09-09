#' @title Modular regressions
#'
#' @description
#' The function allows to automatically specify a number of regression models based on
#' an input of a dependent variable, independent variables, control variables,
#' interaction terms, and model structure.
#'
#' @details
#' The function runs a series of models of \code{type} and \code{familiy}. When
#' \code{type} == "glmer" is selected, the variables defined in \code{fe} are
#' used to compute random intercepts, i.e. "(1 | fe)". First, the function runs a
#' controls only model regressing \code{cv} on \code{dv}. Next, a base model is
#' computed as \code{dv} = f(\code{cv} + \code{iv}). Then a series of interaction
#' models with all combinations of \code{iv} and \code{mv} is computed. If
#' \code{full_model} is set to \code{TRUE}, a full model with all combinations of
#' \code{iv} and \code{mv} in it is added.
#' For \code{type} "glm" the options \code{robust_type == 0},
#' \code{robust_type == 1}, and \code{robust_type == 2} control how the
#' variance-covariance matrix for standard error estimation is computed. The
#' options \code{robust_type == 1} and \code{robust_type == 2} require a cluster
#' variable defined in \code{var_cluster}.
#' \code{type} "glmer" requires one or more variables to compute fixed effects
#' defined in \code{fe}.
#'
#' @param type}{\code{character} variable indicating the model type, takes "glm"
#' or "glmer"
#' @param fam}{\code{character} variable indicating the model familiy, takes:
#' "gaussian" or "binomial"
#' @param dv}{\code{character} variable indicating the name of the dependt
#' variable
#' @param cv}{\code{character} variable or vector thereof indicating the name of
#' the control variables, take \code{NULL} as input
#' @param iv}{\code{character} variable indicating the name of the independt
#' variable
#' @param mv}{\code{character} variable or vector thereof indicating the name of
#' the moderator variables
#' @param fe}{\code{character} variable or vector thereof indicating the name of
#' the fixed effect variables
#' @param robust_type}{Indicator which type of standard errors is to be computed:
#' 0 - no clustering, 1 - clustering using \code{var_cluster}, 2 - clustering using
#' \code{var_cluster} with bootstraping
#' @param var_cluster}{Vector, matrix, or data.frame containing the variables that
#' are used for clustering
#' @param data}{\code{data.frame} used to run the regression models - all variables
#' are taken from \code{data}
#' @param full_model}{\code{logical} variable whether a full model, including all
#' indicated interaction terms, should be computed
#' @param show}{\code{logical} variable indicating whether output is printed to
#' console or returned as \code{data.frame}
#' @param show_cv}{\code{logical} variable indicating whether output should
#' include intercept and control variables
#'
#' @return
#' The output is a list of three elements, named "coefficents", "p.values", and
#' "std.errors". Each of the list elements shows the coeffient estimates, p-values,
#' and standard errors for the computed models, respectively.
#' When the option \code{show} is set to \code{TRUE} the output is printed to the
#' console. If the package \code{knitr} is available, the output is printed using
#' \code{knitr::kable}. When \code{show} and \code{show_cv} are set to \code{TRUE}
#' the output is printed using package \code{stargazer}.
#'
#' @examples
#' data <- create_data()
#'
#' modular_regressions(
#'   type = "glm", fam = "gaussian", dv = "ebit",
#'   cv = c("advertising", "assets", "sic"), iv = "revenue", mv = c("rnd", "staff"),
#'   robust_type = 0, data = data, full_model = TRUE, show = TRUE, show_cv = FALSE
#' )
#' modular_regressions(
#'   type = "glm", fam = "gaussian", dv = "ebit",
#'   cv = c("advertising", "assets", "sic"), iv = "revenue", mv = c("rnd", "staff"),
#'   robust_type = 2, var_cluster = "country", data = data,
#'   full_model = FALSE, show = TRUE, show_cv = FALSE
#' )
#' modular_regressions(
#'   type = "glmer", fam = "gaussian", dv = "ebit",
#'   cv = c("advertising", "assets", "sic"), iv = "revenue", mv = c("rnd", "staff"),
#'   fe = "country", data = data, full_model = TRUE, show = FALSE, show_cv = FALSE
#' )
#'
#' modular_regressions(
#'   type = "glm", fam = "binomial", dv = "female_ceo",
#'   cv = c("advertising", "assets", "sic"), iv = "revenue", mv = c("rnd", "staff"),
#'   robust_type = 0, var_cluster = NULL, data = data,
#'   full_model = TRUE, show = TRUE, show_cv = TRUE
#' )
#' modular_regressions(
#'   type = "glm", fam = "binomial", dv = "female_ceo",
#'   cv = c("advertising", "assets", "sic"), iv = "revenue", mv = c("rnd", "staff"),
#'   robust_type = 2, var_cluster = "country", data = data,
#'   full_model = FALSE, show = TRUE, show_cv = FALSE
#' )
#' modular_regressions(
#'   type = "glmer", fam = "binomial", dv = "female_ceo",
#'   iv = "revenue", mv = c("rnd", "staff"), fe = "country",
#'   data = data, full_model = TRUE, show = TRUE, show_cv = FALSE
#' )
#' @export
#' @importFrom stats na.omit

modular_regressions <- function(type = "glm", fam = "gaussian", dv, cv = NULL, iv, mv, fe = NULL, robust_type = 0, var_cluster = NULL, data, full_model = TRUE, show = TRUE, show_cv = FALSE) {
  if (type == "glm") {
    model_parameters <- list(dv = dv, cv = cv, iv = iv, mv = mv, data = data, full_model = full_model)
    class(model_parameters) <- c("param_glm", class(model_parameters))
    attr(model_parameters, "family") <- fam
  } else if (type == "glmer") {
    model_parameters <- list(dv = dv, cv = cv, iv = iv, mv = mv, fe = fe, data = data, full_model = full_model)
    class(model_parameters) <- c("param_glmer", class(model_parameters))
    attr(model_parameters, "family") <- fam
  }

  out <- .hlpr_modular_models(parameters = model_parameters)

  if (robust_type != 0) {
    var_cluster <- as.data.frame(data)[, var_cluster]
  }

  out <- .hlpr_modular_report(model_list = out, cv = cv, show_cv = show_cv, robust_type = robust_type, var_cluster = var_cluster)

  if (show) {
    if (requireNamespace("stargazer", quietly = TRUE)) {
      if (show_cv) {
        std_errors <- vector(mode = "list", length = ncol(out$std.errors) - 1)
        for (i in seq(ncol(out$std.errors) - 1)) {
          std_errors[[i]] <- na.omit(as.data.frame(out$std.errors)[, i + 1])
        }
        stargazer::stargazer(out$models, se = std_errors, type = "text", report = "vc*p", model.numbers = FALSE, intercept.bottom = FALSE, single.row = TRUE)
      } else {
        warning("Stargazer output requires 'show_cv' == TRUE!")
        if (requireNamespace("knitr", quietly = TRUE)) {
          lapply(out[-4], function(x) knitr::kable(x, digits = 3))
        } else {
          lapply(out[-4], print)
        }
      }
    } else if (requireNamespace("knitr", quietly = TRUE)) {
      lapply(out[-4], function(x) knitr::kable(x, digits = 3))
    } else {
      lapply(out[-4], print)
    }
  } else {
    return(out)
  }
}
