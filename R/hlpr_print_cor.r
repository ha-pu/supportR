#' @title print_cor
#'
#' @keywords internal
#' @importFrom insight format_value
#' @importFrom insight format_table
#' @importFrom parameters format_bf
#' @importFrom parameters format_p
#' @importFrom parameters format_pd

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
      p[, nums] <- sapply(p[, nums], format_p, stars_only = TRUE)
    } else if (type == "pd") {
      p[, nums] <- sapply(p[, nums], format_pd, stars_only = TRUE)
    } else if (type == "BF") {
      p[, nums] <- sapply(p[, nums], format_bf, stars_only = TRUE)
    }

    # Round and eventually add stars
    object[, nums] <- sapply(as.data.frame(object)[, nums], format_value, digits = digits)
    if (stars) {
      object[, nums] <- paste0(as.matrix(as.data.frame(object)[, nums]), as.matrix(p[, nums]))
    }
  } else {
    object[, nums] <- sapply(as.data.frame(object)[, nums], format_value, digits = digits)
  }

  if (html) {
    if (requireNamespace("stargazer", quietly = TRUE)) {
      out <- stargazer::stargazer(object, type = "html", summary = FALSE, rownames = FALSE)
      out <- gsub("\\* ", "*", out)
      out <- cat(out, sep = "\n")
      tf <- tempfile()
      sink(tf)
      out
      sink()
      out <- readLines(tf)[-1]
      out <- out[seq(length(out) / 2)[-length(out) / 2]]
      return(out)
    } else {
      stop("html == TRUE requires 'stargazer' package!")
    }
  } else {
    cat(format_table(object))
  }
}
