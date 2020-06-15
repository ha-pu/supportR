# model_vcov generic function ----
model_vcov <- function(model, ...) UseMethod("model_vcov", model)

# model_vcov lm and glm method ----
model_vcov.lm <- model_vcov.glm <- function(model, type = 0, var_cluster = NULL, ...) {
  
  # run checks ----
  if (!is.null(var_cluster) & nrow(model$model) != length(var_cluster)) stop("When using var_cluster, nrow(model$model) must equal length(var_cluster)!")
  
  # compute variance covariance matrix ----
  mat <- vcov(model)
  if(type == 0) {
    vcov_mat <- mat
  } else {
    if (type == 1 & !is.null(var_cluster)) {
      vcov_mat <- multiwayvcov::cluster.vcov(model, var_cluster)
    } else if (type == 2 & !is.null(var_cluster)) {
      vcov_mat <- multiwayvcov::cluster.boot(model, var_cluster, R = 1000, boot_type = "wild", wild_type = function() sample(c(-1, 1), 1))
    }
    colnames(vcov_mat) <- colnames(mat)
    row.names(vcov_mat) <- row.names(mat)
  }
  out <- list(model = model, vcov_mat = vcov_mat)
  
  # set attributes ----
  if (type == 0) {
    attr(out, "type") <- "standard"
    attr(out, "var_cluster") <- NULL
  } else if(type == 1){
    attr(out, "type") <- "robust"
    attr(out, "var_cluster") <- var_cluster
  } else if(type == 2){
    attr(out, "type") <- "robust_boot"
    attr(out, "var_cluster") <- var_cluster
  }
  class(out) <- c("mod_vcov", class(out))
  
  return(out)
}
