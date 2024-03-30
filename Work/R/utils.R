scale_recover <- function(data, method = 'scale', param_df = NULL) {
  results <- list()
  data_res <- data
  if (!is.null(param_df)) {
    if (method=='scale') {
      data_res[] <- scale(data, center=param_df$mean, scale=param_df$std)
    } else if (method=='recover') {
      data_res[] <- t(t(data)*param_df$std+param_df$mean)
    }
  } else {
    if (method=='scale') {
      param_df <- data.frame(mean=sapply(data, function(x) mean(x, na.rm=T)), 
                             std=sapply(data, function(x) sd(x, na.rm=T)))
      data_res[] <- scale(data, center=param_df$mean, scale=param_df$std)
    } else {stop('no param_df found for recover...')}
  }
  results[[1]] <- data_res
  results[[2]] <- param_df
  return(results)
}


