# modular glm ----
modular_glm <- function (fam, dv, cv, iv, mv, data) {
  if (!is.null(cv)) {
    form_cv <- formula(paste0(dv, "~", paste0(cv, collapse = "+")))
    form_iv <- formula(paste0(dv, "~", paste0(cv, collapse = "+"), "+", iv))
    
    cnt_mod <- 3 + length(mv) * 2
    out_mod <- vector(mode = "list", length = cnt_mod)
    
    out_mod[[1]] <- glm(formula = form_cv, data = data, family = fam)
    out_mod[[2]] <- glm(formula = form_iv, data = data, family = fam)
    
    for (i in seq(length(mv))) {
      j <- i * 2 + 1
      form_mv <- formula(paste0(dv, "~", paste0(cv, collapse = "+"), "+", iv, "+", mv[i]))
      out_mod[[j]] <- glm(formula = form_mv, data = data, family = fam)
      j <- j + 1
      form_mv <- formula(paste0(dv, "~", paste0(cv, collapse = "+"), "+", iv, "+", mv[i], "+", paste0(c(iv, mv[i]), collapse = ":")))
      out_mod[[j]] <- glm(formula = form_mv, data = data, family = fam)
    }
    
    form_full <- formula(paste0(dv, "~", paste0(cv, collapse = "+"), "+", iv, "+", paste0(mv, collapse = "+"), "+", paste1(element1 = iv, element2 = mv, sep = ":", collapse = "+")))
    out_mod[[cnt_mod]] <- glm(formula = form_full, data = data, family = fam)
  } else {
    form_iv <- formula(paste0(dv, "~", iv))
    
    cnt_mod <- 2 + length(mv) * 2
    out_mod <- vector(mode = "list", length = cnt_mod)
    
    out_mod[[1]] <- glm(formula = form_iv, data = data, family = fam)
    
    for (i in seq(length(mv))) {
      j <- i * 2
      form_mv <- formula(paste0(dv, "~", paste0(cv, collapse = "+"), "+", iv, "+", mv[i]))
      out_mod[[j]] <- glm(formula = form_mv, data = data, family = fam)
      j <- j + 1
      form_mv <- formula(paste0(dv, "~", paste0(cv, collapse = "+"), "+", iv, "+", mv[i], "+", paste0(c(iv, mv[i]), collapse = ":")))
      out_mod[[j]] <- glm(formula = form_mv, data = data, family = fam)
    }
    
    form_full <- formula(paste0(dv, "~", iv, "+", paste0(mv, collapse = "+"), "+", paste1(element1 = iv, element2 = mv, sep = ":", collapse = "+")))
    out_mod[[cnt_mod]] <- glm(formula = form_full, data = data, family = fam)
  }
  
  class(out_mod) <- "modular_glm"
  return(out_mod)
}

# modular glmer ----
modular_glmer <- function (fam, dv, cv, iv, mv, fe, data) {
  fe <- paste0("(1|", fe, ")")
  
  if (!is.null(cv)) {
    cv <- c(cv, fe)
  } else {
    cv <- fe
  }
  
  form_cv <- formula(paste0(dv, "~", paste0(cv, collapse = "+")))
  form_iv <- formula(paste0(dv, "~", paste0(cv, collapse = "+"), "+", iv))
  
  cnt_mod <- 3 + length(mv) * 2
  out_mod <- vector(mode = "list", length = cnt_mod)
  
  if (fam == "gaussian") {
    out_mod[[1]] <- lme4::lmer(formula = form_cv, data = data)
    out_mod[[2]] <- lme4::lmer(formula = form_iv, data = data)
  } else if (fam == "binomial") {
    out_mod[[1]] <- lme4::glmer(formula = form_cv, data = data, fam = fam)
    out_mod[[2]] <- lme4::glmer(formula = form_iv, data = data, fam = fam) 
  }
  
  for (i in seq(length(mv))) {
    j <- i * 2 + 1
    form_mv <- formula(paste0(dv, "~", paste0(cv, collapse = "+"), "+", iv, "+", mv[i]))
    if (fam == "gaussian") {
      out_mod[[j]] <- lme4::lmer(formula = form_mv, data = data)
    } else if (fam == "binomial") {
      out_mod[[j]] <- lme4::glmer(formula = form_mv, data = data, fam = fam)
    }
    
    j <- j + 1
    form_mv <- formula(paste0(dv, "~", paste0(cv, collapse = "+"), "+", iv, "+", mv[i], "+", paste0(c(iv, mv[i]), collapse = ":")))
    if (fam == "gaussian") {
      out_mod[[j]] <- lme4::lmer(formula = form_mv, data = data)
    } else if (fam == "binomial") {
      out_mod[[j]] <- lme4::glmer(formula = form_mv, data = data, fam = fam)
    }
  }
  
  form_full <- formula(paste0(dv, "~", paste0(cv, collapse = "+"), "+", iv, "+", paste0(mv, collapse = "+"), "+", paste1(element1 = iv, element2 = mv, sep = ":", collapse = "+")))
  if (fam == "gaussian") {
    out_mod[[cnt_mod]] <- lme4::lmer(formula = form_full, data = data)
  } else if (fam == "binomial") {
    out_mod[[cnt_mod]] <- lme4::glmer(formula = form_full, data = data, fam = fam)
  }
  
  class(out_mod) <- "modular_glmer"
  return(out_mod)
}
