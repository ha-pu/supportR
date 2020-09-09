#' @title modular_models
#'
#' @aliases
#' .hlpr_modular_models
#' .hlpr_modular_models.param_glm
#' .hlpr_modular_models.param_glmer
#'
#' @keywords internal
#' @importFrom lme4 glmer
#' @importFrom lme4 lmer
#' @importFrom stats formula
#' @importFrom stats glm
#' @importFrom stats update

.hlpr_modular_models <- function(parameters, ...) UseMethod(".hlpr_modular_models", parameters)

#' @method .hlpr_modular_models param_glm
#' @keywords internal

.hlpr_modular_models.param_glm <- function(parameters, ...) {
  dv <- parameters$dv
  cv <- parameters$cv
  iv <- parameters$iv
  mv <- parameters$mv
  data <- parameters$data

  if (is.null(cv)) {
    cv <- 1
  }

  out <- vector(mode = "list", length = 3 + length(mv) * 2)
  form_cv <- formula(paste0(dv, "~", paste0(cv, collapse = "+")))
  out[[1]] <- glm(formula = form_cv, data = data, family = attr(parameters, "family"))

  form_iv <- formula(paste0(dv, "~", paste0(cv, collapse = "+"), "+", iv))
  out[[2]] <- update(out[[1]], formula. = form_iv)

  for (i in seq(length(mv))) {
    j <- i * 2 + 1
    form_mv <- formula(paste0(dv, "~", paste0(cv, collapse = "+"), "+", iv, "+", mv[i]))
    out[[j]] <- update(out[[1]], formula. = form_mv)
    j <- j + 1
    form_mv <- formula(paste0(dv, "~", paste0(cv, collapse = "+"), "+", iv, "+", mv[i], "+", paste0(c(iv, mv[i]), collapse = ":")))
    out[[j]] <- update(out[[1]], formula. = form_mv)
  }

  if (parameters$full_model) {
    form_full <- formula(paste0(dv, "~", paste0(cv, collapse = "+"), "+", iv, "+", paste0(mv, collapse = "+"), "+", .hlpr_paste(element1 = iv, element2 = mv, sep = ":", collapse = "+")))
    out[[length(out)]] <- update(out[[1]], formula. = form_full)
  } else {
    out <- out[-length(out)]
  }

  return(out)
}

#' @method .hlpr_modular_models param_glmer
#' @keywords internal

.hlpr_modular_models.param_glmer <- function(parameters, ...) {
  dv <- parameters$dv
  cv <- parameters$cv
  iv <- parameters$iv
  mv <- parameters$mv
  fe <- paste0("(1|", parameters$fe, ")")
  data <- parameters$data

  if (!is.null(cv)) {
    cv <- c(cv, fe)
  } else {
    cv <- fe
  }

  out <- vector(mode = "list", length = 3 + length(mv) * 2)
  form_cv <- formula(paste0(dv, "~", paste0(cv, collapse = "+")))
  if (attr(parameters, "family") == "gaussian") {
    out[[1]] <- lmer(formula = form_cv, data = data)
  } else {
    out[[1]] <- glmer(formula = form_cv, data = data, fam = attr(parameters, "family"))
  }

  form_iv <- formula(paste0(dv, "~", paste0(cv, collapse = "+"), "+", iv))
  out[[2]] <- update(out[[1]], formula. = form_iv)

  for (i in seq(length(mv))) {
    j <- i * 2 + 1
    form_mv <- formula(paste0(dv, "~", paste0(cv, collapse = "+"), "+", iv, "+", mv[i]))
    out[[j]] <- update(out[[1]], formula. = form_mv)
    j <- j + 1
    form_mv <- formula(paste0(dv, "~", paste0(cv, collapse = "+"), "+", iv, "+", mv[i], "+", paste0(c(iv, mv[i]), collapse = ":")))
    out[[j]] <- update(out[[1]], formula. = form_mv)
  }

  if (parameters$full_model) {
    form_full <- formula(paste0(dv, "~", paste0(cv, collapse = "+"), "+", iv, "+", paste0(mv, collapse = "+"), "+", .hlpr_paste(element1 = iv, element2 = mv, sep = ":", collapse = "+")))
    out[[length(out)]] <- update(out[[1]], formula. = form_full)
  } else {
    out <- out[-length(out)]
  }

  return(out)
}
