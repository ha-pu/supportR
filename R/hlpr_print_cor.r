# .hlpr_print_cor ----
.hlpr_print_cor <- function(object, digits = 2, stars = TRUE, html = FALSE) {
  nums <- sapply(as.data.frame(object), is.numeric)

  # Find attributes
  p <- attributes(object)

  if ("stars" %in% names(p)) {
    stars <- p$stars
  }

  # Significance
  type <- names(p)[names(p) %in% c("BF", "pd", "p")][1]
  p <- p[[type]]

  if (!is.null(p)) {
    if (type == "p") {
      p[, nums] <- sapply(p[, nums], parameters::format_p, stars_only = TRUE)
    } else if (type == "pd") {
      p[, nums] <- sapply(p[, nums], parameters::format_pd, stars_only = TRUE)
    } else if (type == "BF") {
      p[, nums] <- sapply(p[, nums], parameters::format_bf, stars_only = TRUE)
    }

    # Round and eventually add stars
    object[, nums] <- sapply(as.data.frame(object)[, nums], insight::format_value, digits = digits)
    if (stars) {
      object[, nums] <- paste0(as.matrix(as.data.frame(object)[, nums]), as.matrix(p[, nums]))
    }
  } else {
    object[, nums] <- sapply(as.data.frame(object)[, nums], insight::format_value, digits = digits)
  }

  if (html) {
    out <- stargazer::stargazer(object, type = "html", summary = FALSE, rownames = FALSE)
    out <- gsub("\\* ", "*", out)
    out <- cat(out, sep = "\n")
    tf <- tempfile()
    sink(tf)
    print_correlation(data_cor, print = TRUE, html = TRUE)
    sink()
    out <- read_lines(tf, skip = 1)
    out <- out[seq(length(out) / 2)[-length(out) / 2]]
    return(out)
  } else {
    cat(insight::format_table(object))
  }
}
