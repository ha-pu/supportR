# data_summary generic function ----
data_summary <- function(input, ...) UseMethod("data_summary", input)

# data_summary data.frame method ----
data_summary.data.frame <- function(input, at, show = TRUE, ...) {
  if (is.numeric(at)) {
    if (is.double(at)) {
      message("'at' of class 'double' is coerced to class 'integer'!")
      at <- as.integer(at)
    }
    at <- names(input)[at]
  }
  if (is.character(at)) {
    input <- as.data.frame(input)
    out <- lapply(at, function(x) .hlpr_get_summary(input[,x]))
    out <- cbind.data.frame(out)
    colnames(out) <- at
    out$Statistic <- stat

    out <- tidyr::gather(out, -Statistic, key = "Variable", value = "value")
    out <- tidyr::spread(out, key = Statistic, value = value)

    out <- out[c("Variable", stat)]
    out[,stat[-1]] <- sapply(out[,stat[-1]], as.numeric)

    if (show) {
      .hlpr_print_summary(input = out)
    } else {
      return(out)
    }
  } else {
   stop("'at' must be class 'character' or 'numeric'!")
  }
}

# data_summary grouped_df method ----
data_summary.grouped_df <- function(input, at, show = TRUE, ...) {
  if (ncol(attr(input,"groups")) > 2) {
    warning("'data_summary' uses only the first group for clustering!")
  }
  out <- lapply(attr(input, "groups")$.rows, function(x) {
    out <- data_summary(input = dplyr::ungroup(input[x,]), at = at, show = FALSE)
    return(out)
  })
  out <- dplyr::bind_rows(out)
  out$Cluster <- paste(names(attr(input,"groups"))[[1]], rep(as.data.frame(attr(input,"groups"))[,1], each = length(at)), sep = ": ")
  out <- out[,c("Variable", "Cluster", stat)]
  out <- out[order(out$Variable, out$Cluster),]

  if (show) {
    .hlpr_print_summary(input = out)
  } else {
    if (requireNamespace("tibble", quietly = TRUE)) {
      out <- tibble::as_tibble(out)
	}
    return(out)
  }
}

# data_summary numeric, logical, character and factor method ----
data_summary.numeric <- data_summary.logical <- data_summary.character <- data_summary.factor <- function(input, show = TRUE, ...) {
  input <- as.data.frame(input)
  out <- data_summary(input = input, at = "input", show = FALSE)

  if (show) {
    .hlpr_print_summary(input = out)
  } else {
    return(out)
  }
}

stat <- c("Type", "n", "SD", "Min.", "1st Qu.", "Mean", "Median", "3rd Qu.", "Max.", "Groups")
