# .hlpr_get_summary generic function ----
.hlpr_get_summary <- function(input, ...) UseMethod(".hlpr_get_summary", input)

# .hlpr_get_summary numeric method ----
.hlpr_get_summary.numeric <- function(input, ...) {
  input <- input[!is.na(input)]
  out <- c(
    .hlpr_get_stats(input),
    NA
  )
  return(out)
}

# .hlpr_get_summary logical method ----
.hlpr_get_summary.logical <- function(input, ...) {
  input <- input[!is.na(input)]
  out <- c(
    .hlpr_get_stats(input),
    .hlpr_get_uniques(input)
  )
  return(out)
}

# .hlpr_get_summary character and factor method ----
.hlpr_get_summary.character <- .hlpr_get_summary.factor <- function(input, ...) {
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
    .hlpr_get_uniques(input)
  )
  return(out)
}
