#' @title Prepare model summary
#'
#' #' @aliases
#' model_summary
#' model_summary.mod_vcov
#' model_summary.lm
#' model_summary.glm
#' model_summary.lmerMod
#' model_summary.glmerMod
#'
#' @description
#' The function \code{model_summary} computes coefficient estimates for linear
#' regression models or linear fixed effects models. For linear models, the user
#' can provide a cluster variable to cluster the standard errors and can select
#' to use bootstraping for clustering.
#'
#' @details
#' For input of class "lm" or "glm" the options \code{type == 0},  \code{type == 1},
#' and \code{type == 2} control how the variance-covariance matrix for standard
#' error estimation is computed. The options \code{type == 1} and \code{type == 2}
#' require a cluster variable defined in \code{var_cluster}.
#'
#' @param input Object of class "mod_vcov", linear model of class "lm" or "glm",
#' or linear fixed effects model of class "lmerMod" or "glmerMod"
#' @param type Indicator which type of standard errors is to be computed:
#' 0 - no clustering, 1 - clustering using \code{var_cluster}, 2 - clustering
#' using \code{var_cluster} with bootstraping
#' @param var_cluster Vector, matrix, or data.frame containing the variables
#' that are used for clustering
#' @param randfe \code{logical} variable indicating whether random fixed effects
#' are provided for models of class "lmerMod" or "glmerMod"
#' @param show \code{logical} variable indicating whether output is printed to
#' console or returned as \code{data.frame}
#' @param sg \code{logical} variable indicating whether output is printed in html
#' format or as standard output
#'
#' @return
#' Coefficient estimates for the model provided as \code{"input"} based on model class and
#' selection in \code{type}. When the option \code{show} is set to \code{TRUE} the output
#' is printed to the console. If the package \code{knitr} is available, the output is
#' printed using \code{knitr::kable}. When \code{sg} is set to \code{TRUE} the output is
#' printed in html format (this overrules \code{show == FALSE}). When \code{show} is
#' set to \code{FALSE} the output is returned as \code{data.frame} or \code{tibble}
#' (when package \code{tibble} is available). For models of class "lmerMod" and
#' "glmerMod" and \code{randfe == TRUE}, random fixed effects are either  printed
#' (\code{show == TRUE}) or returned as part of a named list containing
#' \code{Variables} = coefficient estimates and \code{RandFE} containing the
#' random fixed effects.
#'
#' @seealso \code{\link{model_vcov}}
#'
#' @examples
#' data <- create_data()
#'
#' mod1 <- lm(ebit ~ advertising + assets + revenue + international * rnd, data = data)
#' mod1_vcov <- model_vcov(model = mod1, type = 1, var_cluster = data$country)
#' model_summary(input = mod1_vcov, show = TRUE, sg = FALSE)
#' model_summary(input = mod1, type = 0, var_cluster = NULL, show = TRUE, sg = FALSE)
#'
#' mod2 <- glm(female_ceo ~ advertising + assets + revenue + international * rnd,
#'   data = data, family = "binomial"
#' )
#' model_summary(input = mod2, type = 2, var_cluster = data$country, show = TRUE, sg = FALSE)
#'
#' mod3 <- lme4::lmer(ebit ~ advertising + assets + revenue + international * rnd +
#'   (1 | country), data = data)
#' model_summary(input = mod3, randfe = TRUE, show = FALSE, sg = FALSE)
#'
#' mod4 <- lme4::glmer(female_ceo ~ advertising + assets + revenue + international * rnd +
#'   (1 | country), data = data, family = "binomial")
#' model_summary(input = mod4, randfe = FALSE, show = FALSE, sg = TRUE)
#' @rdname model_summary
#' @export
#' @importFrom dplyr bind_rows
#' @importFrom lme4 ranef
#' @importFrom lmtest coeftest
#' @importFrom Matrix diag
#' @importFrom purrr map
#' @importFrom stats pt
#' @importFrom stats vcov

model_summary <- function(input, ...) UseMethod("model_summary", input)

#' @rdname model_summary
#' @method model_summary mod_vcov
#' @export

model_summary.mod_vcov <- function(input, show = TRUE, sg = FALSE, ...) {

  # get data ----
  model <- input$model
  vcov_mat <- input$vcov_mat

  # compute se and p-values ----
  coef <- model$coefficients
  se <- coeftest(model, vcov_mat)[, 2]
  t_value <- coef / se
  p_value <- 2 * pt(-abs(t_value), df = model$df.residual)

  # export data ----
  out <- data.frame(
    Variables = names(coef),
    Estimate = coef,
    Std_Error = se,
    t_value,
    p_value,
    row.names = NULL
  )
  class(out) <- c("model_summary", class(out))

  # prepare output ----
  if (show & sg) {
    warning("'show' == TRUE overrules 'sg' == TRUE")
  }
  if (show) {
    print(x = out)
  } else {
    if (sg) {
      out <- list(model, out$se)
    } else if (requireNamespace("tibble", quietly = TRUE)) {
      out <- tibble::as_tibble(out)
    }
    return(out)
  }
}

#' @rdname model_summary
#' @method model_summary lm
#' @export

model_summary.lm <- function(input, type = 0, var_cluster = NULL, show = TRUE, sg = FALSE, ...) {
  out <- model_vcov(model = input, type = type, var_cluster = var_cluster)
  out <- model_summary(input = out, show = FALSE)
  class(out) <- c("model_summary", class(out))

  # prepare output ----
  if (show & sg) {
    message("'show' == TRUE overrules 'sg' == TRUE")
  }
  if (show) {
    print(x = out)
  } else {
    if (sg) {
      out <- list(input, out$Std_Error)
    } else if (requireNamespace("tibble", quietly = TRUE)) {
      out <- tibble::as_tibble(out)
    }
    return(out)
  }
}

#' @rdname model_summary
#' @method model_summary glm
#' @export

model_summary.glm <- model_summary.lm

#' @rdname model_summary
#' @method model_summary lmerMod
#' @export

model_summary.lmerMod <- function(input, randfe = FALSE, show = TRUE, sg = FALSE, ...) {

  # compute se and p-values ----
  name <- attributes(input@pp$X)$dimnames[[2]]
  coef <- input@beta
  se <- sqrt(diag(vcov(input)))
  t_value <- coef / se
  p_value <- 2 * pt(-abs(t_value), df = nrow(input@frame) - 1)
  out <- data.frame(
    Variables = name,
    Estimate = coef,
    Std_Error = se,
    t_value,
    p_value,
    row.names = NULL
  )
  class(out) <- c("model_summary", class(out))

  # get random fixed effects ----
  if (randfe) {
    out_randfe <- ranef(input)
    out_randfe <- map(seq(length(out_randfe)), ~ {
      out <- as.data.frame(out_randfe[[.x]])
      names(out) <- "Estimate"
      out$Group <- row.names(out)
      row.names(out) <- NULL
      out$Effect <- names(out_randfe)[[.x]]
      out <- out[, c(3:1)]
      return(out)
    })
    out_randfe <- bind_rows(out_randfe)
    class(out_randfe) <- c("model_randfe", class(out_randfe))
  }


  # prepare output ----
  if (show & sg) {
    message("'show' == TRUE overrules 'sg' == TRUE")
  }
  if (show) {
    if (!randfe) {
      print(x = out)
    } else {
      map(list(Variables = out, RandFE = out_randfe), print)
    }
  } else {
    if (sg) {
      out <- list(input)
    } else if (requireNamespace("tibble", quietly = TRUE)) {
      out <- tibble::as_tibble(out)
      if (randfe) {
        out_randfe <- tibble::as_tibble(out_randfe)
      }
    } else {
      class(out) <- "data.frame"
      class(out_randfe) <- "data.frame"
    }
    if (!randfe) {
      return(out)
    } else if (sg & randfe) {
      return(out)
      message("'sg' == TRUE overrules 'randfe' == TRUE")
    } else {
      out <- list(Variables = out, RandFE = out_randfe)
      return(out)
    }
  }
}

#' @rdname model_summary
#' @method model_summary glmerMod
#' @export

model_summary.glmerMod <- model_summary.lmerMod
