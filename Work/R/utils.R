# Signal drift and batch correction
batch_correction <- function(dataset_exp, order_col, batch_col, qc_col, qc_label) {
  
  M <- sb_corr(
    order_col = order_col,
    batch_col = batch_col,
    qc_col = qc_col,
    qc_label = qc_label,
    use_log = TRUE,
    spar_lim = c(-1.5, 1.5),
    min_qc=4
    )
  
  M <- model_apply(M, dataset_exp)
  
  return(predicted(M))
}


factor_sample_col <- function(dataset_exp, col) {
  dataset_exp$sample_meta[, col] <- lapply(dataset_exp$sample_meta[, col], factor)
  return(dataset_exp)
}
  
  ### THIS IS FOR PLOTING THE SIGNAL DRIFT AND BATCH CORRECTION #### \
  # BUT WE COULD USE A PCA PLOT INSTEAD
  
  # C = feature_profile(
  #   run_order=order_col,
  #   qc_label=qc_label,
  #   qc_column=qc_col,
  #   colour_by='batch_qc',
  #   feature_to_plot=,
  #   plot_sd=FALSE
  # )
  

