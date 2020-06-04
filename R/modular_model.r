# helper functions ----
paste1 <- function (element1, element2, sep, collapse) {
  out <- c()
  for (i in element2) {
    for (j in element1) {
      out <- c(out, paste(i, j, sep = sep))
    }
  }
  out <- paste(out, collapse = collapse)
  return(out)
}

# top function ----
modular_model <- function(type = "glm", fam = "gaussian", dv, cv = NULL, iv, mv, fe, cluster = NULL, cluster_boot = FALSE, data, show = TRUE, show_cv = FALSE) {
  if (type == "glm") {
    out <- modular_glm(fam = fam, dv = dv, cv = cv, iv = iv, mv = mv, data = data)
  } else if (type == "glmer") {
    out <- modular_glmer(fam = fam, dv = dv, cv = cv, iv = iv, mv = mv, fe = fe, data = data)
  } else {
    print(warning("This model type is currently not supported!"))
  }
  report(model_list = out, cv = cv, cluster = cluster, cluster_boot = cluster_boot, data = data, show = show, show_cv = show_cv)
}
