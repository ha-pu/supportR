# data_summary generic function ----
data_summary <- function(input, ...) UseMethod("data_summary", input)

# data_summary data.frame method ----
data_summary.data.frame <- function(input, at, show = TRUE, ...) {
  if (is.numeric(at)) {
    if (is.double(at)) {
      warning("'at' of class 'double' is coerced to class 'integer'!")
      at <- as.integer(at)
    }
    at <- names(input)[at]
  }
  if (is.character(at)) {
    input <- as.data.frame(input)
    out <- lapply(at, function(x) .get_summary(input[,x]))
    out <- cbind.data.frame(out)
    colnames(out) <- at
    out$Statistic <- stat

    out <- tidyr::gather(out, -Statistic, key = "Variable", value = "value")
    out <- tidyr::spread(out, key = Statistic, value = value)

    out <- out[c("Variable", stat)]
    out[,stat[-1]] <- sapply(out[,stat[-1]], as.numeric)

    if (show) {
      .print_data_summary(input = out)
    } else {
      return(out)
    }
  } else {
   warning("'at' must be class 'character' or 'numeric'!")
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
    .print_data_summary(input = out)
  } else {
    return(out)
  }
}

# data_summary numeric, logical, character and factor method ----
data_summary.numeric <- data_summary.logical <- data_summary.character <- data_summary.factor <- function(input, show = TRUE, ...) {
  input <- as.data.frame(input)
  out <- data_summary(input = input, at = "input", show = FALSE)

  if (show) {
    .print_data_summary(input = out)
  } else {
    return(out)
  }
}

# .get_summary generic function ----
.get_summary <- function(input, ...) UseMethod(".get_summary", input)

# .get_summary numeric method ----
.get_summary.numeric <- function(input, ...) {
  input <- input[!is.na(input)]
  out <- c(
    .get_stats(input),
    NA
  )
  return(out)
}

# .get_summary logical method ----
.get_summary.logical <- function(input, ...) {
  input <- input[!is.na(input)]
  out <- c(
    .get_stats(input),
    .get_uniques(input)
  )
  return(out)
}

# .get_summary character and factor method ----
.get_summary.character <- .get_summary.factor <- function(input, ...) {
  input <- input[!is.na(input)]
  out <- c(
    class(input),
    length(input),
    NA,
    NA,
    NA,
    NA,
    NA,
    NA,
    NA,
    .get_uniques(input)
  )
  return(out)
}

# refactor functions for .get_summary ----
.get_stats <- function(input) {
  out <- c(
    class(input),
    length(input),
    sd(input),
    min(input),
    quantile(input, 0.25),
    mean(input),
    median(input),
    quantile(input, 0.75),
    max(input)
  )
  return(out)
}

.get_uniques <- function(input) {
  out <- length(unique(input))
  return(out)
}

stat <- c("Type", "n", "SD", "Min.", "1st Qu.", "Mean", "Median", "3rd Qu.", "Max.", "Groups")

.print_data_summary <- function(input) {
  if (requireNamespace("knitr", quietly = TRUE)) {
    knitr::kable(input, digits = 3)
  } else {
    input[,stat] <- round(input[,stat], digits = 3)
    print(input)
  }
}
