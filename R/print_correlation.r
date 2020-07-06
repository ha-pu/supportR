# print_correlation generic function ----
print_correlation <- function(data, ...) UseMethod("print_correlation", data)

# print_correlation data.frame method ----
print_correlation.data.frame <- function(data, show = TRUE, html = FALSE, ...) {

  # run correlation ----
  cor <- correlation::correlation(data)

  # do summary ----
  if (show | html) {
    out <- .do_summary_correlation(object = cor)
  }

  # return output ----
  if(html) {
    if (requireNamespace("stargazer", quietly = TRUE)) {
      if (!show) warning("html == TRUE overrules show == FALSE!")
      out <- .do_print_correlation(object = out, html = TRUE)
      return(out)
    } else {
      warning("html == TRUE requires 'stargazer' package!")
    }
  } else if (show) {
    .do_print_correlation(object = out)
  } else {
    return(cor)
  }
}

# .do_summary_correlation [helper] function ----
.do_summary_correlation <- function(object, stars = TRUE) {

  frame <- .get_matrix(object)

  target_col <- names(object)[names(object) %in% c("r", "rho", "tau", "Median")][1]
  if (is.na(target_col)) {
    target_col <- names(object)[!names(object) %in% c("Parameter1", "Parameter2")][1]
  }

  out <- .create_matrix(frame, object, column = target_col)

  # Fill attributes
  for (i in names(object)[!names(object) %in% c("Group", "Parameter1", "Parameter2", target_col)]) {
    attri <- .create_matrix(frame, object, column = i)
    attr(out, i) <- attri
  }

  # Transfer attributes
  attributes(out) <- c(attributes(out), attributes(object)[!names(attributes(object)) %in% c("names", "row.names", "class", names(attributes(out)))])
  attr(out, "stars") <- stars
  attr(out, "coefficient_name") <- target_col

  return(out)
}

# .do_print_correlation [helper] function ----
.do_print_correlation <- function(object, digits = 2, stars = TRUE, html = FALSE) {
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
    return(out)
  } else {
    cat(insight::format_table(object))
  }
}

# .get_matrix [helper] function ----
.get_matrix <- function (data) {
  if ((all(data$Parameter1 %in% data$Parameter2) && all(data$Parameter2 %in% data$Parameter1))) {
    vars <- as.character(unique(c(data$Parameter1, data$Parameter2)))
    dim <- length(vars)
    m <- matrix(nrow = dim, ncol = dim, dimnames = list(vars, vars))
  }
  else {
    m <- matrix(nrow = length(unique(data$Parameter1)), ncol = length(unique(data$Parameter2)), dimnames = list(unique(data$Parameter1), unique(data$Parameter2)))
  }
  m[] <- 1

  return(m)
}

# .fill_matrix [helper] function ----
.fill_matrix <- function(frame, object, column = "r") {
  for (row in row.names(frame)) {
    for (col in colnames(frame)) {
      frame[row, col] <- object[(object$Parameter1 == row & object$Parameter2 == col) | (object$Parameter2 == row & object$Parameter1 == col), column][1]
    }
  }

  # Add Parameter column
  frame <- as.data.frame(frame)
  frame$Parameter <- row.names(frame)
  frame <- frame[c("Parameter", names(frame)[names(frame) != "Parameter"])]
  row.names(frame) <- NULL

  # Remove upper triangular
  frame[-1][lower.tri(frame[-1])] <- NA
  frame <- frame[c(1, ncol(frame):2)]

  return(frame)
}

# .create_matrix [helper] function ----
.create_matrix <- function(frame, object, column = "r") {
  if ("Group" %in% names(object)) {
    out <- data.frame()
    for (g in unique(object$Group)) {
      data <- object[object$Group == g, ]
      m <- .fill_matrix(frame, data, column = column)
      m$Group <- g
      out <- rbind(out, m)
    }
    out <- out[c("Group", names(out)[names(out) != "Group"])]
  } else {
    out <- .fill_matrix(frame, object, column = column)
  }

  return(out)
}
