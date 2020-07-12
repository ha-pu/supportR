# trunc_rnorm ----
trunc_rnorm <- function(n, mean = 0, sd = 1, lwr = -1, upr = 1, nnorm = n * 1.1) {
  message(nnorm)
  samp <- rnorm(n = nnorm, mean = mean, sd = sd)
  samp <- samp[samp >= lwr & samp <= upr]
  if (length(samp) >= n) {
    return(sample(samp, n))
  } else {
    trunc_rnorm(n = n, mean = mean, sd = sd, lwr = lwr, upr = upr, nnorm = nnorm * 1.1)
  }
}
